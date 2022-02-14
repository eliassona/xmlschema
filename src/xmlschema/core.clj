(ns xmlschema.core
  (:require [instaparse.core :as insta]
            [clojure.xml :as xml])
  (:use [clojure.pprint]))

(defmacro dbg [body]
  `(let [x# ~body]
     (println "dbg:" '~body "=" x#)
     x#))

(defn html->hiccup
   [html]
   (if-not (string? html)
     (->> (map html->hiccup (:content html))
       (concat [(:tag html) (:attrs html)])
       (keep identity)
       vec)
     html))

(defn hiccup-of [text]
  (let [enlive (xml/parse (java.io.ByteArrayInputStream. (.getBytes text)))]
    (html->hiccup enlive)))

(def parser (insta/parser (clojure.java.io/resource "xmlschema.bnf")))

(def env 
  {:simpleTypes 
   {"string" (fn [_ value] {:result true, :value value}),
    "integer" (fn [_ value] 
                (let [v (read-string value)]
                  {:result (number? v), 
                   :value (dbg v)}))}})

(defn same-elements? [c]
  (= (count (set c))) 1)

(defn simple-type-restriction [arg-map & conditions]
  (when (not (same-elements? (map (fn [c] (-> c meta :type)) conditions)))
    (throw (IllegalArgumentException.)))
  (let [env (gensym)
        value (gensym)
        base (gensym)
        logic-expr (-> conditions first meta :type)]
  `(fn [~env ~value]
     (if-let [~base (((:simpleTypes ~env) (~arg-map :base)) ~env ~value)]
      (if (:result ~base)
        {:result 
         ~(condp = logic-expr
            :or `(or ~@(map (fn [c] `(:result (~c ~env (:value ~base)))) conditions))
            :and `(and ~@(map (fn [c] `(:result (~c ~env (:value ~base)))) conditions))
            ),
         :value (:value ~base)} 
        ~base)
      (throw (IllegalArgumentException. "Unknown base"))))))


(defn enumeration [arg-map]
  (with-meta `(fn [env# value#]
               (when-let [exp-value# (:value ~arg-map)]
                 {:result (= value# exp-value#), :value value#})) {:type :or}))



(defn max-inclusive [arg-map]
  (with-meta `(fn [env# value#]
               (when-let [exp-value# (~arg-map :value)]
                 {:result (<= value# exp-value#), :value value#})) {:type :and}))
(defn min-inclusive [arg-map]
  (with-meta `(fn [env# value#]
               (when-let [exp-value# (~arg-map :value)]
                 {:result (>= value# exp-value#), :value value#})) {:type :and}))
(defn max-exclusive [arg-map]
  (with-meta `(fn [env# value#]
               (when-let [exp-value# (~arg-map :value)]
                 {:result (< value# exp-value#), :value value#})) {:type :and}))
(defn min-exclusive [arg-map]
  (with-meta `(fn [env# value#]
               (when-let [exp-value# (~arg-map :value)]
                 {:result (>= value# exp-value#), :value value#})) {:type :and}))

(def ast->clj-map  
  {
   :ident (fn[& chars] (read-string (apply str chars))) 
   :string-literal (fn[& chars] (apply str (-> chars rest butlast)))
   :attrs (fn [& args] (apply hash-map args))
   :enumeration enumeration
   :simpleType-restriction simple-type-restriction
   :maxInclusive max-inclusive
   :minInclusive min-inclusive
   :maxExclusive max-exclusive
   :minExclusive min-exclusive
   })

(defn ast->clj [ast]
    (insta/transform
    ast->clj-map 
    ast))






