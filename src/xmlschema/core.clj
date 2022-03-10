(ns xmlschema.core
  (:require [instaparse.core :as insta]
            [clojure.xml :as xml])
  (:use [clojure.pprint]))

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

   (defn simpleType-restriction [arg-map & conditions]
     (assert-req-attrs arg-map :base)
     (do-throw! (not (same-elements? (map (fn [c] (-> c meta :type)) conditions))) "not the same type") 
     (let [env (gensym)
           value (gensym)
           base-result (gensym)
           base-value (gensym)
           logic-expr (if (empty? conditions) :empty (-> conditions first meta :type))]
     `(fn ([~env ~value]
        (if-let [[~base-result ~base-value] ((~env ~(type-name-of (arg-map :base))) ~env ~value)]
         [(and ~base-result
             ~(condp = logic-expr
                :empty true
                :or `(or ~@(map (fn [c] `(~c ~env ~base-value)) conditions))
                :and `(and ~@(map (fn [c] `(~c ~env ~base-value)) conditions))
                )) ~base-value] 
         (arg-exception! "Unknown base")))
        ([~env] ((~env (~arg-map :base)) ~env)))))


   
   (defn enumeration [arg-map]
     (with-meta `(fn [env# value#]
                  (when-let [exp-value# (:value ~arg-map)]
                    (= value# exp-value#))) {:type :or}))


   (defn numeric-op-expr [arg-map op]
       (when-let [exp-value (:value arg-map)]
         (with-meta 
           `(fn [env# value#]
              (~op value# ~(Long/valueOf exp-value))) {:type :and}))) ;TODO make better cast here

   (defn max-inclusive [arg-map]
     (numeric-op-expr arg-map `<=))

   (defn min-inclusive [arg-map]
     (numeric-op-expr arg-map `>=))

   (defn max-exclusive [arg-map]
     (numeric-op-expr arg-map `<))

   (defn min-exclusive [arg-map]
     (numeric-op-expr arg-map `>))

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
     (if (= (-> type-fn meta :type) :complexType)
       `[~name ~@value] 
        [name value]))
   
   (defn element [& args] 
     `(let [[arg-map# type-fn#] (normalize-args [~@args])
            n# (-> arg-map# :name keyword)
            ref# (:ref arg-map#)
            type-name# (:type arg-map#)
            default# (:default arg-map#)
            fixed# (:fixed arg-map#)]
        (do-throw! (and ref# (or n# type-name#)) "ref and name type cannot be used at the same time")
        (do-throw! (and default# fixed#) "default and fixed cannot be used at the same time")
        (add-meta
          (fn ([env# [tmp# & value# :as all#]]
             (cond 
               ref#
               (let [r# (env# ref#)]
                 (r# env# all#))
               type-name#   
               (if-let [t# (env# (type-name-of type-name#))]
                 (massage-return-value n# t# (t# env# (prepare-value t# value# default# fixed#)))
                 (arg-exception! (format "Unknown type: %s" type-name#)))
               :else
               (massage-return-value n# type-fn# (type-fn# env# (prepare-value type-fn# value# default# fixed#)))))
            ([env#]
              (cond 
                ref#
                ((env# ref#) env#)
                type-name#
                (elements-of env# n# (env# type-name#))
                :else
                (elements-of env# n# type-fn#))))
          :element n#)))

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
   
   
   (defn schema [& elements]
     `(let [[arg-map# & root-objects#] (normalize-args [~@elements])
            elements# (filter (fn [e#] (= (:type (meta e#)) :element)) root-objects#)
            elem-map# (apply merge (map (fn [e#] {(-> e# meta :name) e#}) elements#))
            env# (merge ~`env (apply merge (map (fn [e#] {(name-of e#) e#}) (only-named-objects root-objects#))))
            imports# (filter #(= (-> % meta :type) :import) root-objects#)
            import-env# (apply merge (map #(-> % meta :env)  imports#))
            env# (apply merge env# import-env#)
            env-key-set# (set (keys env#))
            ]
          (with-meta 
            (fn 
              ([xml#]
                  ((elem-map# (first xml#)) env# xml#))
              ([]
                {:elements 
                 (filter identity
                         (map
                           (fn [e#] (e# env#)) elements#))
                 :env env-key-set#})) {:type :schema, :xmlns (ns-of arg-map#), :env env#})))

   (defn simpleType [& args]
     `(let [[arg-map# type-fn#] (normalize-args [~@args])
            n# (:name arg-map#)]
        (add-meta
          (condp = n# 
            "string"
            (fn ([env# value#] [true value#])
               ([env#] "string"))
            "integer"
            (fn ([env# value#]
              (try 
                (let [v# (Long/valueOf value#)]
                  [true v#])
                (catch NumberFormatException e#
                  [false value#])))
                ([env#] "integer"))
            "boolean"
            (fn ([env# value#] 
                  (let [v# (read-string value#)]
                    (if (boolean? v#) [true v#] [false value#])))
               ([env#] "boolean"))
            (fn 
              ([env# value#] (type-fn# env# value#))
              ([env#] (if n# n# (type-fn# env#))))) 
          :simpleType n#)))

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
   
   (defn xml-schema-list [& args]
     `(let [[arg-map# & elements#] (normalize-args [~@args])
            itemType# (:itemType arg-map#)
            ]
        (assert-req-attrs arg-map# :itemType)
        (add-meta 
          (fn ([env# value#]
            (let [e# (env# itemType#)]
              (cons true (map #(e# env# %) (.split value# " ")))))
            ([env#] itemType#))
          :list nil)
        ))

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
     (let [pd (partition-by keyword data)]
       
     ))
   
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
   
   (defn choice [& args]
     `(let [args# (normalize-args [~@args])
            [min-occurs# max-occurs#] (min-max-occurs-of (first args#))
            m# (make-map (rest args#))
            ]
        (add-meta 
			    (fn ([env# value#]
			      (let [result# (get-result m# env# value#) 
			            names# (map first result#)
			            s# (set names#)
			            n# ((occurance-of names#) (first s#))
			            ]
			          (conj 
			            result#
			            (and 
			              (< (count s#) 2) 
			              (and (>= n# min-occurs#) (<= n# max-occurs#))))))
			      ([env#] (flatten (all-sequence-items env# (rest args#)))))
       :choice)))
   
   (defn schema-sequence [& args]
     `(let [args# (normalize-args [~@args])
            [min-occurs# max-occurs#] (min-max-occurs-of (first args#))
            m# (make-map (rest args#))
            ]
       (add-meta 
         (fn ([env# value#]
           (let [result# (get-result m# env# value#) 

                 ]
               (conj 
                 result#
                 true)))
         ([env#] (flatten (all-sequence-items env# (rest args#)))))
         :sequence)))
 
   (defn all [& args]
     `(let [args# (normalize-args [~@args])
            [min-occurs# max-occurs#] (min-max-occurs-of (first args#))
            m# (make-map (rest args#))
            ]
        (add-meta 
          (fn ([env# value#]
                   (let [result# (get-result m# env# value#) 
                         ]
                       (conj 
                         result#
                         true)))
            ([env#] (flatten (all-sequence-items env# (rest args#)))))
          :all)))
   
   (def complex-type-sub-element #{:simpleContent 
                                   :complexContent 
                                   :group :all 
                                   :choice :sequence}) 
   (def attrs-sub-element #{:attribute :attributeGroup})
   
   (defn sub-element-of [args]
     (first (filter (fn [e] (contains? complex-type-sub-element (-> e meta :type))) args)))
   
   (defn attrs-of [args]
     (filter (fn [v] (contains? attrs-sub-element (-> v meta :type))) args))
   
   (defn complexType [& args]
     `(let [args# (normalize-args [~@args])
            arg-map# (first args#)
            n# (:name arg-map#)
            sub-elem# (sub-element-of (rest args#))
            attrs# (attrs-of args#)]
        (add-meta 
          (fn
            ([env# value#]
              (cond (= (-> sub-elem# meta :type) :simpleContent)
                    (sub-elem# env# value#)
                    (map? (first value#))
                    (let [e# (rest value#)
                      a# (first value#)]
                    [(apply merge (map (fn [attr#] (attr# env# a#)) attrs#))
                    (if (empty? e#) [] (sub-elem# env# e#))])
                    :else
                    [(sub-elem# env# value#)]))
            ([env#]
              (if sub-elem#
                (sub-elem# env#)
                [])))
          :complexType n#)))
   
   (defn group [& args]
     `(let [[arg-map# type-fn#] (normalize-args [~@args])
            n# (:name arg-map#)
            ref# (:ref arg-map#)]
        (do-throw! (and n# ref#) "name and ref cannot be used at the same time")
        (add-meta 
          (fn 
            ([env# value#]
              (if ref#
                ((env# ref#) env# value#)
                (type-fn# env# value#)))
            ([env#]
              (type-fn# env#)
              ))
        :group n#)))
   
   (defn attribute [& args]
     `(let [[arg-map# type-fn#] (normalize-args [~@args])
            n# (:name arg-map#)
            t# (:type arg-map#)
            ref# (:ref arg-map#)
            use# (:use arg-map#)
            default# (:default arg-map#)
            fixed# (:fixed arg-map#)
            ]
        (do-throw! (and n# ref#) "name and ref cannot be used at the same time")
        (do-throw! (and default# fixed#) "default and fixed cannot be used at the same time")
        (add-meta
          (fn 
            ([env# value#]
              (let [key# (keyword n#)
                    value# (value# key#)
                    value# (if value# value# default#)]
                (do-throw! (and (not value#) (= use# "required")) (format "required attribute %s is missing" key#))
                (do-throw! (and value# (= use# "prohibited")) (format "attribute %s is not allowed" key#))
                (cond
                  n#
                  {key# (if value#
                          ((if t# (env# t#) type-fn#) env# value#)
                          [true nil])}
                  ref#
                  (if-let [a# (env# ref#)]
                    (if (= (-> a# meta :type) :attribute)
                      (type-fn# env# value#)
                      (arg-exception! "ref must point to an attribute"))
                    (arg-exception! "invalid ref"))
                  :else
                  (arg-exception! "name or ref must be set")
                  )))
                
            ([env#]
              (cond
                ref# ((env# ref#) env#)
                n# [n# (if t# t# (type-fn# env#))])
              ))
        :attribute n#)))
   
   (defn attributeGroup [& args]
     `(let [[arg-map# & type-fn#] (normalize-args [~@args])
            n# (:name arg-map#)
            ref# (:ref arg-map#)]
        (do-throw! (and n# ref#) "name and ref cannot be used at the same time")
        (do-throw! (and ref# (not (empty? type-fn#)))  "ref and attributes cannot be used at the same time")
        (add-meta
          (fn ([env# value#]
            (if ref#
              (let [rf# (env# ref#)]
                (if (= (-> rt# meta :type) :attributeGroup)
                  (rf# env# value#)
                  (arg-exception! "ref does not point to an attributeGroup")))
              (apply merge (map (fn [f#] (f# env# value#)) type-fn#))))
            ([env#]
              (set (apply concat (map (fn [f#] (f# env#)) type-fn#)))))
          :attributeGroup n#)))
   
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
   
   (defn qName-of [xmlns e]
     {(let [k (key e)]
        (if (> (.indexOf k ":") 0)
          k
          (xs-type xmlns k)))
        (val e)})
   
   (defn schema-import [& args]
   `(let [[arg-map# & type-fn#] [~@args]
          f# (:schemaLocation arg-map#)
          schema# (-> f# slurp-file hiccup-of (schema-eval :schema))
          xmlns# (str (-> schema# meta :xmlns))
          schema-env# (apply merge (map (partial qName-of xmlns#) (-> schema# meta :env)))
          l# (count xmlns#)]
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
     `(let [type-name# (:base ~arg-map)
            ]
        (add-meta 
          (fn [env# [attr-value-map# value#]]
            (let [v# ((env# type-name#) env# value#)]
                [(attr-map-of env# ~(vec attrs) attr-value-map#) v#])
              ) :simpleContent-restriction nil)))
   
   (defn simpleContent [& args]
     `(let [[arg-map# type-fn#] (normalize-args [~@args])]
        (add-meta 
          (fn [env# value#]
            (type-fn# env# value#))
          :simpleContent nil)))
   
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

(defn expand-includes ([elements] ;TODO do recursion with loop recur
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
        

(defn schema->clj [hiccup start]
  (let [p (fn [text] (parser text :start start))]
    (-> (if (string? hiccup) (hiccup-of hiccup) hiccup) (expand-includes start) pr-str p ast->clj)))

(defn schema-eval 
  ([hiccup start]
  (eval (schema->clj hiccup start)))
  ([hiccup] (schema-eval hiccup :schema)))
   
(defn layout-of [schema]
  (-> (schema) :elements vec))

(defn parse-predef [predef]
  (ast->clj (parser (pr-str predef) :start :simpleType)))


(def predefs
  [[:simpleType {:name "string"} [:restriction {:base ""}]]
   [:simpleType {:name "integer"} [:restriction {:base ""}]]
   [:simpleType {:name "boolean"} [:restriction {:base ""}]]
   [:simpleType {:name "anyURI"} [:restriction {:base "string"}]] ;TODO
   [:simpleType {:name "base64Binary"} [:restriction {:base "string"}]] ;TODO
   [:simpleType {:name "hexBinary"} [:restriction {:base "string"}]] ;TODO
   [:simpleType {:name "date"} [:restriction {:base "string"}]] ;TODO
   [:simpleType {:name "decimal"} [:restriction {:base "string"}]];TODO 
   [:simpleType {:name "byte"} 
    [:restriction {:base "integer"} 
	    [:minInclusive {:value "-128"}] 
	    [:maxInclusive {:value "127"}]]]
   [:simpleType {:name "short"} 
    [:restriction {:base "integer"} 
	    [:minInclusive {:value "-32768"}] 
	    [:maxInclusive {:value "32767"}]]] 
   [:simpleType {:name "unsignedByte"} 
	   [:restriction {:base "integer"} 
		   [:minInclusive {:value "0"}] 
		   [:maxInclusive {:value "255"}]]] 
   [:simpleType {:name "unsignedShort"} 
	   [:restriction {:base "integer"} 
		   [:minInclusive {:value "0"}] 
		   [:maxInclusive {:value "65535"}]]] 
   [:simpleType {:name "nonPositiveInteger"} 
	   [:restriction {:base "integer"} 
		   [:maxInclusive {:value "0"}]]] 
   [:simpleType {:name "nonNegativeInteger"} 
	   [:restriction {:base "integer"} 
		   [:minInclusive {:value "0"}]]] 
   [:simpleType {:name "positiveInteger"} 
	   [:restriction {:base "integer"} 
		   [:minExclusive {:value "0"}]]] 
   [:simpleType {:name "negativeInteger"} 
	   [:restriction {:base "integer"} 
		   [:maxExclusive {:value "0"}]]]
   ])

(def env
  (apply merge 
   (map 
     (comp (fn [f] {(-> f meta :name) f}) eval parse-predef) 
     predefs)))




