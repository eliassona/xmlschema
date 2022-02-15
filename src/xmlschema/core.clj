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
             type-fn# (element-map# (-> xml# first name))]
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
   :simpleType identity
   :element element
   :schema schema
   })

(defn ast->clj [ast]
    (insta/transform
    ast->clj-map 
    ast))






