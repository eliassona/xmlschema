(ns xmlschema.core
  (:require [instaparse.core :as insta]
            [clojure.xml :as xml])
  (:use [clojure.pprint])
  (:import [java.util.regex Pattern]
           [org.bix.xmlschema CollNormalizer]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defmacro do-throw! [condition msg]
  `(when ~condition
     (throw (IllegalArgumentException. ~msg))))

(defn arg-exception! [msg]
  (throw (IllegalArgumentException. msg)))

(defn xml->hiccup
   [xml]
   (if-not (string? xml)
     (->> (map xml->hiccup (:content xml))
       (concat [(:tag xml) (:attrs xml)])
       (keep identity)
       vec)
     xml))

(defn hiccup-of [text]
  (let [enlive (xml/parse (java.io.ByteArrayInputStream. (.getBytes text)))]
    (xml->hiccup enlive)))

(def parser (insta/parser (clojure.java.io/resource "xmlschema.bnf")))

(defn add-meta 
  ([o t]
    (add-meta o t nil))
  ([o t n]
    (with-meta o (merge {:type t} (if n {:name n} {})))))

(defn assert-req-attrs [arg-map & keys]
  (doseq [k keys]
    (assert (contains? arg-map k) (format "argument %s is missing" k))))

(defn same-elements? [c]
  (= (count (set c))) 1)

(declare ast->clj)

(defn type-name-of [t]
  (ast->clj (parser t :start :type)))

(defn or-fn [args]
  (loop [args (map first args)]
    (if (not (empty? args))
      (if (first args)
        true
        (recur (rest args)))
      false)))

(defn and-fn [args]
  (loop [args (map first args)]
    (if (not (empty? args))
      (if (first args)
        (recur (rest args))
        false)
      true)))

(defn statements-of [env value conditions]
  (first 
    (reduce 
      (fn [[s v] c]
        (let [new-v (c env v)]
          [(conj s new-v) (second new-v)])
        ) [[] value] 
      conditions)))
  
                     

(defn simpleType-restriction-fn [base conds logic-expr arg-map]
  (do-throw! (= base nil) "base attribute must be set")
  (do-throw! (not (same-elements? (map (fn [c] (-> c meta :type)) conds))) "not the same type")
  (let [base-type-name (type-name-of base)]
    (fn ([env value]
         (if-let [[base-result base-value] ((env base-type-name) env value)]
           (let [statements (statements-of env base-value conds)]
             [(and base-result
                   (condp = logic-expr
                     :empty true
                     :or (or-fn statements)
                     :and (and-fn statements)
                     )) 
              (if (= logic-expr :and)
                (-> statements last second)
                base-value)]) 
           (arg-exception! "Unknown base")))
       ([env] ((env (arg-map :base)) env)))))
   
(defn simpleType-restriction [arg-map & conditions]
  `(let [conds# [~@conditions]]
     (simpleType-restriction-fn 
       (:base ~arg-map) 
       conds# 
       (if (empty? conds#) :empty (-> conds# first meta :type)) 
       ~arg-map)))

   
(defn enumeration [arg-map & _]
  (with-meta `(fn [env# value#]
               (when-let [exp-value# (:value ~arg-map)]
                 [(= value# exp-value#) value#])) {:type :or}))


(defn numeric-op-expr [arg-map op]
    (when-let [exp-value (:value arg-map)]
      (with-meta 
        `(fn [env# value#]
           [(~op value# ~(read-string exp-value)) value#]) {:type :and})))

(defn max-inclusive [arg-map & _]
  (numeric-op-expr arg-map `<=))

(defn min-inclusive [arg-map & _]
  (numeric-op-expr arg-map `>=))

(defn max-exclusive [arg-map & _]
  (numeric-op-expr arg-map `<))

(defn min-exclusive [arg-map & _]
  (numeric-op-expr arg-map `>))
   
(defn pattern [arg-map & _]
  (let [m (Pattern/compile (:value arg-map))]
    (with-meta
      `(fn [env# value#]
         [(not= (re-matches ~m value#) nil) value#])
      {:type :or})))
   
(defn totalDigits [arg-map & _]
  (let [n (-> arg-map :value read-string)]
    (with-meta
      `(fn [env# value#]
         [(<= (-> value# str count) ~n) value#]) 
      {:type :and}))
  )
   
(defn min-max-length [arg-map op]
  (let [n (-> arg-map :value read-string)]
    (with-meta
      `(fn [env# value#]
         [(~op (count value#) ~n) value#]) 
      {:type :and})))
   
(defn minLength [arg-map & _] (min-max-length arg-map `>=))
(defn maxLength [arg-map & _] (min-max-length arg-map `<=))
   
(defn fractionDigits [arg-map & _]
  (let [n (-> arg-map :value read-string)]
    (do-throw! (< n 1) "must be bigger than zero")
    (with-meta
      `(fn [env# value#]
         (let [s# (str value#)
               ix# (.indexOf s# ".")
               fr# (.substring s# (if (>= ix# 0) (inc ix#) 0))] 
         [(<= (count fr#) ~n) value#])) 
      {:type :and})))
   

   
(defn whiteSpace [arg-map & _]
  (let [v (:value arg-map)
        whitespace-pattern "[\t\n\r]"]
    (do-throw! (not (contains? #{"collapse" "replace" "preserve"} v)) "invalid value")
    (with-meta
      `(fn [env# value#]
         value#
         [true 
          (condp = ~v
            "replace" (.replaceAll value# ~whitespace-pattern " ")
            "collapse" (.replaceAll value# ~whitespace-pattern "")
            value#)
          ]) 
      {:type :and})))

(defn elements-of [env n type-fn]
  (if-let [t (type-fn env)]
    [n t]
    n))
   
(defn prepare-value [type-fn value default fixed]
  (let [st (= (-> type-fn meta :type) :simpleType)]
    (do-throw! (and (or default fixed) (not st)) "default and fixed can only be used for simpleType")
    (if st
      (let [v (first value)]
        (cond 
          default (if v v default)
          fixed (if (= v fixed) v (arg-exception! (format "is fixed"))) ;TODO better error text
          :else v
          ))
      value)))

(defn massage-return-value [name type-fn value]
  (let [type (-> type-fn meta :type)]
    (with-meta 
      (if (= type :complexType)
        `[~name ~@value] 
        [name value]) 
      {:result (-> value meta :result), :type type})))
   
(defn element-fn [type-fn name ref type-name default fixed]
  (do-throw! (and ref (or name type-name)) "ref and name type cannot be used at the same time")
  (do-throw! (and default fixed) "default and fixed cannot be used at the same time")
  (let [type-name (if type-name (type-name-of type-name) nil)]
    (add-meta
      (fn ([env [tmp & value :as all]]
       (cond 
         ref
         (let [r (env ref)]
           (r env all))
         type-name   
         (if-let [t (env type-name)]
           (massage-return-value name t (t env (prepare-value t value default fixed)))
           (arg-exception! (format "Unknown type: %s" type-name)))
         :else
         (massage-return-value name type-fn (type-fn env (prepare-value type-fn value default fixed)))))
      ([env]
        (cond 
          ref
          ((env ref) env)
          type-name
          (elements-of env name (env type-name))
          :else
          (elements-of env name type-fn))))
     :element name)))
    
(defn element [& args] 
  `(let [[arg-map# type-fn#] (normalize-args [~@args])]
     (element-fn type-fn# 
                 (-> arg-map# :name keyword) 
                 (:ref arg-map#) 
                 (:type arg-map#) 
                 (:default arg-map#) 
                 (:fixed arg-map#))))

(defn type? [expected-type o]
  (= expected-type (-> o meta :type)))
   
(defn name-of [o]
  (if-let [n (-> o meta :name)]
    (name n)
    (arg-exception! "Name attribute missing")))

(def named-root-objects #{:simpleType :complexType 
                          :group :attributeGroup
                          :element :attribute
                          :notation})
   
(defn only-named-objects [root-objects]
  (filter (fn [o] (contains? named-root-objects (-> o meta :type))) root-objects))
   
(defn ns-key-of [t]
 (let [ix (.indexOf t ":")]
   (if (> ix 0)
     (.substring t 0 (inc ix))
     t)))
   
(defn ns-of [arg-map]
  (when-let [xmlns (first (filter #(.startsWith % "xmlns:") (map name (keys arg-map))))]
    (ast->clj (parser xmlns :start :xmlns))))
   
(defn name-spaces-of [xmlns imports]
  imports)
   
(defn invalid-names [names]
  (some #(>= (.indexOf % ":") 0) names))
   
(defn schema-fn [env elem-map elements env-key-set arg-map]
  (do-throw! (invalid-names (map name (keys elem-map))) "can't define an element in another namespace")
  (with-meta 
   (fn 
     ([xml-or-hiccup]
       (let [hiccup (if (string? xml-or-hiccup) (hiccup-of xml-or-hiccup) xml-or-hiccup)]
         ((elem-map (first hiccup)) env hiccup)))
     ([]
       {:elements 
        (set 
          (filter identity
                  (map
                    (fn [e] (e env)) elements)))
        :env env-key-set})) {:type :schema, :xmlns (ns-of arg-map), :env env}))
   
(defn import-env-of [elements]
   (let [imports (filter #(= (-> % meta :type) :import) elements)]
     (apply merge (map #(-> % meta :env) imports))))
   
(defn elem-map-of [elements]
  (reduce (fn [acc e] (assoc acc (-> e meta :name) e)) {} elements))
   
(defn schema [& elements]
  `(let [[arg-map# & root-objects#] (normalize-args [~@elements])
         elements# (filter (fn [e#] (= (:type (meta e#)) :element)) root-objects#)
         elem-map# (elem-map-of elements#)
         env# (merge ~`env (reduce (fn [acc# e#] (assoc acc# (name-of e#) e#)) {} (only-named-objects root-objects#)))
         import-env# (import-env-of root-objects#)]
       (schema-fn (apply merge env# import-env#) 
                  elem-map# 
                  elements# 
                  (set (keys env#)) 
                  arg-map#)))
(defn add-result [v]
  (with-meta v {:result (first v)}))

(defmacro def-hardcoded-type [value-code layout-code]
  `(fn 
     ([~'env ~'value] (add-result ~value-code))
     ([~'env] ~layout-code)))

(defn simpleType-fn [name type-fn]
  (add-meta
   (condp = name 
     "string"
     (def-hardcoded-type [true value] "string")
     "integer"
     (def-hardcoded-type 
       (try 
        (let [v (Long/valueOf value)]
          [true v])
        (catch NumberFormatException e
          [false value])) "integer")
     "decimal"
     (def-hardcoded-type 
       (try 
         (let [v (Double/valueOf value)]
           [true v])
         (catch NumberFormatException e
           [false value])) "decimal")
     "boolean"
     (def-hardcoded-type 
       (let [v (read-string value)]
         (if (boolean? v) [true v] [false value])) "boolean")
     (def-hardcoded-type (type-fn env value) (if name name (type-fn env)))) 
   :simpleType name))
   
(defn simpleType [& args]
  `(let [[arg-map# type-fn#] (normalize-args [~@args])
         n# (:name arg-map#)]
     (simpleType-fn n# type-fn#)))

(defn keyref [& [arg-map]] 
  (assert-req-attrs arg-map :name :refer))

(defn extension [& arg-map]
  (assert-req-attrs arg-map :base))

(defn field [& [arg-map]] 
  (assert-req-attrs arg-map :xpath))

(defn selector [& [arg-map]] 
  (assert-req-attrs arg-map :xpath))
   
(defn include [arg-map & args] 
  (assert-req-attrs arg-map :schemaLocation)
  (vals arg-map))
   
   
(defn xml-schema-list-fn [itemType]
  (let [itemType (type-name-of itemType)]
    (add-meta 
         (fn ([env value]
           (let [e (env itemType)] 
             (cons true (map #(e env %) (.split value " ")))))
           ([env] itemType))
         :list nil)))
   
(defn xml-schema-list [& args]
  `(let [[arg-map# & elements#] (normalize-args [~@args])
         itemType# (:itemType arg-map#)]
     (assert-req-attrs arg-map# :itemType)
     (xml-schema-list-fn itemType#)))

(defn notation [& [arg-map]] 
  (assert-req-attrs arg-map :name :public))
   
(defn redefine [& [arg-map]] 
  (assert-req-attrs arg-map :schemaLocation))
   
(defn unique [& [arg-map]] 
  (assert-req-attrs arg-map :name))
   
(defn val-or-default [value default]
  (if value value default))
   
(defn min-max-occurs-of ([arg-map min-default max-default]
  [(val-or-default (:minOccurs arg-map) min-default)
   (val-or-default (:maxOccurs arg-map) max-default)])
  ([arg-map]
    (min-max-occurs-of  arg-map 0 1)))
   
(defn occurance-of [l]
  (reduce (fn [acc v] (update acc v (fn [v] (if v (inc v) 1)))) {} l))
   
   
(defn valid-sequence? [data ks min-occurs max-occurs]
  (let [pd (partition-by keyword data)]))
   
(defn filter-elements 
  ([m op]
    (filter (fn [e] (op (-> e meta :type) :element)) m))
  ([m] (filter-elements m =)))

(defn normalize-args [args]
  (if (-> args first map?) args (cons {} args)))
   
(defn make-map [args]
  (reduce 
    (fn [acc type-fn] (assoc acc (-> type-fn meta :name) type-fn)) {} (filter-elements args)))
   
(defn get-result [m env value]
  (map 
    (fn [v] 
      (let [n (first v)]
        ((n m) env v))) value))
   
(defn extract-name [env arg]
  (let [t (-> arg meta :type)]
    (if (= t :element)
      (-> arg meta :name)
      (arg env))))
   
(defn all-sequence-items [env args]
  (map (partial extract-name env) args))

(defn coll-name-of [element]
  (if (= (-> element meta :type) :element)
    (-> element meta :name)
    (-> element meta :names flatten)))

(defn coll-names-of [elements]
  (map coll-name-of elements))

(defn normalize-coll-data [data names]
  (CollNormalizer/normalizeCollData data names)
  )

(defn flatten-coll-value [value]
  (reduce 
    (fn [acc v] acc (if (-> v first keyword?) (conj acc v) (apply conj acc (rest v)))) 
    [] 
    value)
  )

(defn coll-ok? [value elements]
  (and
    ;TODO same length
    (every? identity (map #(if (-> % first keyword?) (-> % second first) (first %)) value))))


(defn collections [args coll-fn]
  `(let [[arg-map# & elements#] (normalize-args [~@args])
         [min-occurs# max-occurs#] (min-max-occurs-of arg-map#)
         the-map# (make-map elements#)]
     (~coll-fn the-map# min-occurs# max-occurs# elements# (coll-names-of elements#))))

(defn choice-fn [the-map min-occurs max-occurs elements coll-names]
  (let [element-fn #(= (-> % meta :type) :element)
        pure-elem (filter element-fn elements)
        sub-elem (filter #(not (element-fn %)) elements)
        the-keys (-> the-map keys set)]
    (with-meta 
      (fn ([env value]
        (let [value (normalize-coll-data value coll-names)
              value-fn #(-> % first keyword?)
              pure-v (filter value-fn value)
              valid-v (filter #(contains? the-keys (first %)) pure-v)
              sub-v (filter #(not (value-fn %)) value)
              choice-result (and (= (count valid-v) 1) (= (count sub-v) (count sub-elem)) (coll-ok? value elements))
              sub-result (map (fn [e v] (e env v)) sub-elem sub-v)
              pure-result ((the-map (-> valid-v first first)) env (first valid-v))]
          (with-meta 
            (if (empty? sub-result)
              [choice-result  
               pure-result]
              (cons choice-result  
               (flatten-coll-value [pure-result sub-result]))) {:result choice-result})))
        ([env] (flatten (all-sequence-items env elements)))) 
      {:names coll-names, :type :choice})))
   
(defn choice [& args] (collections args `choice-fn))



(defn schema-sequence-fn [the-map min-occurs max-occurs elements coll-names]
  (with-meta 
    (fn ([env value]
      (let [value (normalize-coll-data value coll-names)    
            res (map (fn [e v] (e env v)) elements value)
            seq-ok? (coll-ok? res elements)]
        (with-meta (cons seq-ok? (flatten-coll-value res)) {:result seq-ok?})))
    ([env] (flatten (all-sequence-items env elements))))
      {:names coll-names, :type :sequence}))
   
(defn schema-sequence [& args] (collections args `schema-sequence-fn))

(defn all-ok? [value elements]
  (= (count value) (count elements)))

(defn all-fn [the-map min-occurs max-occurs elements coll-names]
  (with-meta 
     (fn ([env value]
      (let [all-ok? (all-ok? value elements)]     
        (with-meta 
          (cons all-ok? (map (fn [v] ((the-map (first v)) env v)) value)) {:result all-ok?})))
       ([env] (flatten (all-sequence-items env elements))))
     {:names coll-names, :type :all}))
   
(defn all [& args] (collections args `all-fn))
   
(def complex-type-sub-element #{:simpleContent 
                                :complexContent 
                                :group :all 
                                :choice :sequence}) 
(def attrs-sub-element #{:attribute :attributeGroup})
   
(defn sub-element-of [args]
  (first (filter (fn [e] (contains? complex-type-sub-element (-> e meta :type))) args)))
   
(defn attrs-of [args]
  (filter (fn [v] (contains? attrs-sub-element (-> v meta :type))) args))

(defn attr-data-of [env in-data attrs]
  (let [a (apply merge (map (fn [attr] (attr env in-data)) attrs))
        result (every? identity (map (comp first val) a))]
    (with-meta a {:result result})
    ))

(defn complexType-fn [name sub-elem attrs]
  (let [type (-> sub-elem meta :type)]
    (add-meta 
     (fn
       ([env value]
         (cond (= type :simpleContent)
               (sub-elem env value)
               (map? (first value))
               (let [e (rest value)
                 a (first value)
                 attr-data (attr-data-of env a attrs)
                 se (if (empty? e) [] (sub-elem env e))]
                 (with-meta 
                   [attr-data se] 
                   {:result (and (-> attr-data meta :result) (if (empty? se) true (-> se meta :result)))}))
               :else
               (let [se (sub-elem env value)]
                 (with-meta [se] {:result (-> se meta :result)}))))
       ([env]
         (if sub-elem
           (sub-elem env)
           [])))
     :complexType name)))
   
(defn complexType [& args]
  `(let [[arg-map# & elements#] (normalize-args [~@args])]
     (complexType-fn 
       (:name arg-map#) 
       (sub-element-of elements#) 
       (attrs-of elements#))))
   
   
(defn group-fn [name ref type-fn]
  (do-throw! (and name ref) "name and ref cannot be used at the same time")
  (add-meta 
         (fn 
           ([env value]
             (if ref
               ((env ref) env value)
               (type-fn env value)))
           ([env]
             (type-fn env)
             ))
       :group name))
   
(defn group [& args]
  `(let [[arg-map# type-fn#] (normalize-args [~@args])]
     (group-fn (:name arg-map#) (:ref arg-map#) type-fn#)))
   
(defn attribute-fn [name ref type-name type-fn default fixed]
 (do-throw! (and name ref) "name and ref cannot be used at the same time")
 (do-throw! (and default fixed) "default and fixed cannot be used at the same time")
  (add-meta
   (fn 
     ([env value]
       (let [key (keyword name)
             value (value key)
             value (if value value default)]
         (do-throw! (and (not value) (= use "required")) (format "required attribute %s is missing" key))
         (do-throw! (and value (= use "prohibited")) (format "attribute %s is not allowed" key))
         (cond
           name
           (let [v (if value
                   ((if type-name (env type-name) type-fn) env value)
                   [true nil])]
             (with-meta {key v} {:result (first v)}))
           ref
           (if-let [a (env ref)]
             (if (= (-> a meta :type) :attribute)
               (type-fn env value)
               (arg-exception! "ref must point to an attribute"))
             (arg-exception! "invalid ref"))
           :else
           (arg-exception! "name or ref must be set")
           )))
            
     ([env]
       (cond
         ref ((env ref) env)
         name [name (if type-name type-name (type-fn env))])
       ))
 :attribute name))
   
(defn attribute [& args]
  `(let [[arg-map# type-fn#] (normalize-args [~@args])]
     (attribute-fn 
       (:name arg-map#) 
       (:ref arg-map#) 
       (:type arg-map#) 
       type-fn# 
       (:default arg-map#) 
       (:fixed arg-map#))))
   
(defn attributeGroup-fn [name ref type-fn]
  (do-throw! (and name ref) "name and ref cannot be used at the same time")
  (do-throw! (and ref (not (empty? type-fn)))  "ref and attributes cannot be used at the same time")
  (add-meta
    (fn ([env value]
      (if ref
        (let [rf (env ref)]
          (if (= (-> rf meta :type) :attributeGroup)
            (rf env value)
            (arg-exception! "ref does not point to an attributeGroup")))
        (apply merge (map (fn [f] (f env value)) type-fn))))
      ([env]
        (set (apply concat (map (fn [f] (f env)) type-fn)))))
    :attributeGroup name)
  )
   
(defn attributeGroup [& args]
  `(let [[arg-map# & type-fn#] (normalize-args [~@args])]
     (attributeGroup-fn (:name arg-map#) (:ref arg-map#) type-fn#)))
   
(defn memberTypes-of [mt]
  (if mt
    (vec (ast->clj (parser mt :start :memberTypes)))
    []))
   
(defn union-or-of [results]
  (if (empty? results) 
    [false nil]
    (loop [r results]
      (if (empty? r)
        (first results)
        (let [e (first r)]
          (if (first e)
            e
            (recur (rest results))))))))
   
(defn union [& args]
  `(let [[arg-map# & type-fn#] (normalize-args [~@args])
         memberTypes# (memberTypes-of (:memberTypes arg-map#))]
     (fn [env# value#]
       (union-or-of (map #(% env# value#) (concat (map env# memberTypes#) type-fn#))))))
   
(defn xs-type 
  ([t] t)
  ([ns t] (str ns ":" t)))

(defn local-import-env-of [local-env element]
  (with-meta 
    (fn 
      ([_ value] (element local-env value))
      ([_] (element local-env))) (meta element)))
   
(defn qName-of [local-env xmlns e]
  {(let [k (key e)]
     (if (> (.indexOf k ":") 0)
       k
       (xs-type xmlns k)))
     (local-env (val e))})
   
(defn schema-import [& args]
`(let [[arg-map# & type-fn#] [~@args]
       f# (:schemaLocation arg-map#)
       schema# (-> f# slurp-file hiccup-of schema-compile)
       schema-env# (-> schema# meta :env)
       local-env# (partial local-import-env-of schema-env#)
       xmlns# (str (-> schema# meta :xmlns))
       schema-env# (apply merge (map (partial qName-of local-env# xmlns#) schema-env#))]
   (with-meta
     (fn [env#]
         (let [s# (:env (schema#))]
           (set (map (partial str xmlns#) s#))))
     {:type :import, :env schema-env#})))

(defn attr-map-of [env attrs attr-value-map]
  (if (empty? attrs)
    {}
    (apply merge (map #(% env attr-value-map) attrs))))
   
(defn simpleContent-extension [arg-map & attrs]
  `(let [type-name# (:base ~arg-map)]
     (add-meta 
       (fn [env# [attr-value-map# value#]]
         (let [v# ((env# type-name#) env# value#)]
             [(attr-map-of env# ~(vec attrs) attr-value-map#) v#])
           ) :simpleContent-restriction)))
   
(defn simpleContent [& args]
  `(let [[arg-map# type-fn#] (normalize-args [~@args])]
     (add-meta 
       (fn [env# value#]
         (type-fn# env# value#))
       :simpleContent)))
   
(def ast->clj-map  
  {
   :ident (fn[& chars] (read-string (apply str chars))) 
   :string-literal (fn [& chars] (apply str chars)) 
   :attrs (fn [& args] (apply hash-map args))
   :req-attrs (fn [& args] (apply hash-map args))
   :enumeration enumeration
   :simpleType-restriction simpleType-restriction
   :maxInclusive max-inclusive
   :minInclusive min-inclusive
   :maxExclusive max-exclusive
   :minExclusive min-exclusive
   :pattern pattern
   :totalDigits totalDigits
   :minLength minLength
   :maxLength maxLength
   :whiteSpace whiteSpace
   :fractionDigits fractionDigits
   :simpleType simpleType
   :element element
   :schema schema
   :keyref keyref
   :extension extension
   :field field
   :include include 
   :list xml-schema-list
   :notation notation
   :redefine redefine
   :selector selector
   :unique unique
   :choice choice
   :sequence schema-sequence
   :all all
   :complexType complexType
   :group group
   :attribute attribute
   :attributeGroup attributeGroup
   :qName (fn[& chars] (apply str chars))
   :memberTypes (fn [& args] (vec args))
   :union union
   :type xs-type
   :import schema-import 
   :xmlns (fn [ns] ns)
   :simpleContent-extension simpleContent-extension
   :simpleContent simpleContent
   })

(defn ast->clj [ast]
    (insta/transform
    ast->clj-map 
    ast))
   
(defn slurp-file [url]
  (slurp (clojure.java.io/resource url)))
   
(declare schema-eval)

(defn filter-includes [elements]
  (filter (fn [[e]] (not= e :include)) elements)) 

(defn expand-includes ([elements] 
  (let [includes (flatten (map ast->clj (filter (fn [i] (= (first i) :include))  elements)))]
    (if (empty? includes)
      []
      (let [hiccups (mapcat (comp (fn [[_ _ & e]] e) hiccup-of slurp-file) includes)
            child-hiccups (expand-includes hiccups)]
            (concat hiccups child-hiccups))))) 
  ([hiccup start]
    (let [[_ arg-map & elements] hiccup]
      (if (= start :schema)
        (let [expanded-elements (expand-includes elements)]
          (vec (cons :schema (cons arg-map (filter-includes (apply conj elements (filter #(not (empty? %)) expanded-elements)))))))
        hiccup))))

(defn trim-xml [xml-hiccup]
  (if (string? xml-hiccup) (hiccup-of (.trim xml-hiccup)) xml-hiccup))

(defn schema->clj 
  ([hiccup start]
  (let [p (fn [text] (parser text :start start))]
    (-> hiccup #_trim-xml (expand-includes start) pr-str p ast->clj)))
  ([hiccup] (schema->clj hiccup :schema)))

(defn schema-eval
  "Compiles an xml string or a hiccup list to a clojure function with the following singature:
   (fn ([hiccup-or-xml]) ; validates xml data
       ([]))             ; layout of elements
   "
  ([hiccup start]
  (eval (schema->clj hiccup start)))
  ([hiccup] (schema-eval hiccup :schema)))


   
(defn layout-of [schema]
  (-> (schema) :elements))

(defn parse [xml]
  (-> xml .trim hiccup-of pr-str (parser :start :schema)))

(defn parse-predef [predef]
  (ast->clj (parser (pr-str predef) :start :simpleType)))

(defmacro def-type [name base & conditions]
  `[:simpleType {:name ~name} [:restriction {:base ~base} ~@conditions]])

(defmacro def-types [& types]
  `(def predefs ~(vec (map (fn [[name base & conditions]] `(def-type ~name ~base ~@conditions)) types)))
)

(def-types 
  ["string" ""]
  ["integer" ""]
  ["boolean" ""]
  ["anyURI" "string"]
  ["base64Binary" "string"]
  ["hexBinary" "string"]
  ["date" "string"]
  ["dateTime" "string"]
  ["duration" "string"]
  ["gDay" "string"]
  ["gMonth" "string"]
  ["gMonthDay" "string"]
  ["gYear" "string"]
  ["gYearMonth" "string"]
  ["time" "string"]
  ["decimal" ""]
  ["byte" "integer" [:minInclusive {:value "-128"}][:maxInclusive {:value "127"}]]
  ["short" "integer" [:minInclusive {:value "-32768"}][:maxInclusive {:value "32767"}]]
  ["unsignedByte" "integer" [:minInclusive {:value "0"}][:maxInclusive {:value "255"}]]
  ["unsignedShort" "integer" [:minInclusive {:value "0"}][:maxInclusive {:value "65535"}]]
  ["nonPositiveInteger" "integer" [:maxInclusive {:value "0"}]]
  ["nonNegativeInteger" "integer" [:minInclusive {:value "0"}]]
  ["positiveInteger" "integer" [:minInclusive {:value "0"}]]
  ["negativeInteger" "integer" [:maxInclusive {:value "0"}]])

(def env
  (apply merge 
   (map 
     (comp (fn [f] {(-> f meta :name) f}) eval parse-predef) 
     predefs)))


(defn name-only [e]
  (let [k (first e)
        n (name k)]
    (if (.startsWith n "xs:")
      (keyword (.substring n 3))
      k)
    ))

(defn make-import-env [unnamed-elements]
  (let [imports (filter #(= (name-only %) :import) unnamed-elements)
        import-fn (comp #(-> % meta :env) #(schema-eval % :import))]
    (apply merge (map import-fn imports))))

(defn make-new-env [named-elements]
  (reduce (fn [acc e] (assoc acc (-> e second :name) (schema-eval e (name-only e)))) {} named-elements))

(defn schema-compile
  "Does the same as schema-eval but compiles each element inside a schema separately to avoid one big function"
  [hiccup-or-xml]
  (let [hiccup (trim-xml hiccup-or-xml)
        arg-map (second hiccup)
        hiccup-elements (-> (expand-includes hiccup :schema) rest rest)
        filter-fn #(contains? named-root-objects (name-only %))
        named-elements (filter filter-fn hiccup-elements)
        unnamed-elements (filter #(not (filter-fn %)) hiccup-elements)
        import-env (make-import-env unnamed-elements)
        new-env (merge 
                  env 
                  import-env 
                  (make-new-env named-elements))
        elements (filter #(= (-> % meta :type) :element) (vals new-env))  
        elem-map (elem-map-of elements)
        env-key-set (set (keys new-env))]
    (schema-fn new-env elem-map elements env-key-set arg-map)))

(defn is-valid? [result] (-> result meta :result))

(defn map-of [m]
  (reduce (fn [acc e] (assoc acc (key e) (-> e val second))) {} m))

(defn java-friendly-map-of [m]
  (reduce (fn [acc e] (assoc acc (-> e key name) (-> e val))) {} m))

(defn make-java-friendly [hiccup]
  (if (and (coll? hiccup) (not (empty? hiccup))) 
    (let [s (second hiccup)
          n (-> hiccup first name)]
        (if (map? s) 
           (cons n (cons (java-friendly-map-of s) (map make-java-friendly (-> hiccup rest rest))))
           (cons n (map make-java-friendly (rest hiccup)))))
    hiccup))

(defn as-hiccup [result]
  (if (= (-> result meta :type) :simpleType)
    [(first result)(-> result second second)]
    (if (map? (second result))
      (cons (first result) (cons (map-of (second result)) (map as-hiccup (-> result (nth 2) rest))))
      (cons (first result) (map as-hiccup (-> result second rest))))))
  
  


