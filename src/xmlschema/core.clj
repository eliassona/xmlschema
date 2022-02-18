(ns xmlschema.core
  (:require [instaparse.core :as insta]
            [clojure.xml :as xml])
  (:use [clojure.pprint]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

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

(def env 
   {"string" (with-meta (fn [_ value] [true value]) {:type :simpleType}) 
    "integer" (with-meta (fn [_ value]
                (try 
                  (let [v (Long/valueOf value)]
                    [true v])
                  (catch NumberFormatException e
                    [false value]))) {:type :simpleType})
    })

 

   (defn assert-req-attrs [arg-map & keys]
     (doseq [k keys]
       (assert (contains? arg-map k) (format "argument %s is missing" k))))


   (defn same-elements? [c]
     (= (count (set c))) 1)

   (defn simple-type-restriction [arg-map & conditions]
     (assert-req-attrs arg-map :base)
     (when (not (same-elements? (map (fn [c] (-> c meta :type)) conditions)))
       (throw (IllegalArgumentException.)))
     (let [env (gensym)
           value (gensym)
           base-result (gensym)
           base-value (gensym)
           logic-expr (-> conditions first meta :type)]
     `(fn [~env ~value]
        (if-let [[~base-result ~base-value] ((~env ~(arg-map :base)) ~env ~value)]
         [(and ~base-result
             ~(condp = logic-expr
                :or `(or ~@(map (fn [c] `(~c ~env ~base-value)) conditions))
                :and `(and ~@(map (fn [c] `(~c ~env ~base-value)) conditions))
                )) ~base-value] 
         (throw (IllegalArgumentException. "Unknown base"))))))


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




   (defn element 
     ([arg-map type]
     [(:name arg-map) type :element]
      )
     ([arg-map]
       (if-let [type-name (:type arg-map)]
         `[~(:name arg-map) 
           (fn [env# value#] 
             (if-let [t# (env# ~type-name)]
               (t# env# value#)
               (throw (IllegalArgumentException. "Unknown type"))))
           :element]))) ;TODO      
   
   


   (defn schema [& elements]
     (let [m 
       (apply 
         hash-map 
         (mapcat 
           (fn [[name type-fn]] [name type-fn]) 
           (filter (fn [[_ _ t]] (= t :element)) elements)))]
       `(fn [xml#]
          (let [element-map# ~m
                [element# attrs# data#] xml#
                type-fn# (element-map# (-> attrs# :name name))]
            ;type-fn returns binary!!!!
          ))))
   (defn simple-type 
     ([type-fn] type-fn)
     ([arg-map type-fn] [(:name arg-map) type-fn]))
   

   (defn keyref [& [arg-map]] 
     (assert-req-attrs arg-map :name :refer))

   (defn extension [& arg-map]
     (assert-req-attrs arg-map :base))

   (defn field [& [arg-map]] 
     (assert-req-attrs arg-map :xpath))
   (defn selector [& [arg-map]] 
     (assert-req-attrs arg-map :xpath))
   
   (defn include [& [_ arg-map]] 
     (assert-req-attrs arg-map :schemaLocation))

   (defn xml-schema-list [& args]
     (let [[arg-map] args]
       (if (= (count args) 1)
         (assert-req-attrs arg-map :itemType)
         (comment "TODO")
         )))

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
   
   
   (defn choice [& args]
     (let [args (if (-> args first map?) args (cons {} args))
           [min-occurs max-occurs] (min-max-occurs-of (first args))]
       `(fn [env# value#]
          (let [m# ~(reduce (fn [acc [name type-fn]] (assoc acc name type-fn)) {} (rest args))
                result# (map 
                          (fn [[name# data#]] 
                            [name# ((m# (name name#)) env# data#)]) value#)
                names# (map first result#)
                s# (set names#)
                n# ((occurance-of names#) (first s#))]
            (conj 
              result#
              (and 
                (< (count s#) 2) 
                (and (>= n# ~min-occurs) (<= n# ~max-occurs))))
            
            ))))
   
   (defn schema-sequence [& args]
     (let [args (if (-> args first map?) args (cons {} args))
           [min-occurs max-occurs] (min-max-occurs-of (first args))]
       `(fn [env# value#]
          (let [m# ~(reduce (fn [acc [name type-fn]] (assoc acc name type-fn)) {} (rest args))
                result# (map 
                          (fn [[name# data#]] 
                            [name# ((m# (name name#)) env# data#)]) value#)
                names# (map first result#)
                s# (set names#)
                ]
            (conj 
              result#
              true   ;TODO
              )
            
            ))))
   (defn all [& args]
     (let [args (if (-> args first map?) args (cons {} args))
           [min-occurs max-occurs] (min-max-occurs-of (first args))]
       `(fn [env# value#]
          (let [m# ~(reduce (fn [acc [name type-fn]] (assoc acc name type-fn)) {} (rest args))
                result# (map 
                          (fn [[name# data#]] 
                            [name# ((m# (name name#)) env# data#)]) value#)
                names# (map first result#)
                s# (set names#)
                ]
            (conj 
              result#
              true   ;TODO
              )
            
            ))))
   
   
   (def ast->clj-map  
     {
      :ident (fn[& chars] (read-string (apply str chars))) 
      :string-literal (fn [& chars] (apply str chars)) 
      :attrs (fn [& args] (apply hash-map args))
      :enumeration enumeration
      :simpleType-restriction simple-type-restriction
      :maxInclusive max-inclusive
      :minInclusive min-inclusive
      :maxExclusive max-exclusive
      :minExclusive min-exclusive
      :simpleType simple-type
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
      })

   (defn ast->clj [ast]
       (insta/transform
       ast->clj-map 
       ast))

(defn schema->clj [hiccup start]
  (let [p (fn [text] (parser text :start start))]
    (-> hiccup pr-str p ast->clj)))

(defn schema-eval [hiccup start]
  (eval (schema->clj hiccup start)))
   
   
(defn parse-predef [predef]
  (ast->clj (parser (pr-str predef) :start :simpleType)))

(defn eval-predef [[name type-fn]]
  [name (eval (with-meta type-fn {:type :simpleType}))])

(def predefs
  [
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
		   [:maxExclusive {:value "0"}]]]])

(def env
  (merge env (apply 
        hash-map 
        (mapcat 
          (comp eval-predef parse-predef) 
          predefs))))

