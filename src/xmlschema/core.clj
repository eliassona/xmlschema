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
  {:simpleTypes 
   {"string" (fn [_ value] [true value])
    "integer" (fn [_ value]
                (try 
                  (let [v (Long/valueOf value)]
                    [true v])
                  (catch NumberFormatException e
                    [false value])))
                    
                    
                        
    }})

 



   (defn same-elements? [c]
     (= (count (set c))) 1)

   (defn simple-type-restriction [arg-map & conditions]
     (when (not (same-elements? (map (fn [c] (-> c meta :type)) conditions)))
       (throw (IllegalArgumentException.)))
     (let [env (gensym)
           value (gensym)
           base-result (gensym)
           base-value (gensym)
           logic-expr (-> conditions first meta :type)]
     `(fn [~env ~value]
        (if-let [[~base-result ~base-value] (((:simpleTypes ~env) ~(arg-map :base)) ~env ~value)]
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




   (defn element [arg-map type]
     [(:name arg-map) type :element]
      )

   (defn schema [& elements]
     (let [m 
       (apply 
         hash-map 
         (mapcat 
           (fn [[name type-fn]] [name type-fn]) 
           (filter (fn [e] (= (nth e 2) :element)) elements)))]
       `(fn [xml#]
          (let [element-map# ~m
                [element# attrs# data#] xml#
                type-fn# (dbg (element-map# (-> attrs# :name name)))]
            ;type-fn returns binary!!!!
          ))))
   (defn simple-type 
     ([type-fn] type-fn)
     ([arg-map type-fn] [(:name arg-map) type-fn]))
   
   (defn assert-req-attrs [arg-map & keys]
     (doseq [k keys]
       (assert (contains? arg-map k) (format "argument %s is missing" k))))

   (defn keyref [& [_ arg-map]] 
     (assert-req-attrs arg-map :name :refer))

   (defn extension [& arg-map]
     (dbg arg-map)
     #_(assert-req-attrs (dbg arg-map) :base))
   
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
      })

   (defn ast->clj [ast]
       (insta/transform
       ast->clj-map 
       ast))


(defn parse-predef [predef]
  (ast->clj (parser (pr-str predef) :start :simpleType)))

(defn eval-predef [[name type-fn]]
  [name (eval type-fn)])

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
  (assoc env 
   :simpleTypes 
   (merge 
     (:simpleTypes env)
      (apply 
        hash-map 
        (mapcat 
          (comp eval-predef parse-predef) 
          predefs)))))


