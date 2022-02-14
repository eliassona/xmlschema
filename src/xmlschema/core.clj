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
   {"string" (fn [_ value] true),
    "integer" (fn [_ value] true)}})

(defn same-elements? [c]
  (= (count (set c))) 1)

(defn simple-type-restriction [arg-map & conditions]
  (when (not (same-elements? (map (fn [c] (-> c meta :type)) conditions)))
    (throw (IllegalArgumentException.)))
  (let [env (gensym)
        value (gensym)
        logic-expr (-> conditions first meta :type)]
  `(fn [~env ~value]
     (if-let [base# (((:simpleTypes ~env) (~arg-map :base)) ~env ~value)]
      (and base#
         ~(condp = logic-expr
            :or `(or ~@(map (fn [c] `(~c ~env ~value)) conditions))
            :and `(and ~@(map (fn [c] `(~c ~env ~value)) conditions))
            )) 
      (throw (IllegalArgumentException. "Unknown base"))))))


(defn enumeration [arg-map]
  (with-meta `(fn [env# value#]
               (when-let [exp-value# (:value ~arg-map)]
                 (= value# exp-value#))) {:type :or}))


(defn op-exp [arg-map op]
  (when-let [exp-value (:value arg-map)]
    (with-meta 
      `(fn [env# value#]
         (~op value# ~exp-value)) {:type :and})))

(defn max-inclusive [arg-map]
  (op-expr arg-map `<=))

(defn min-inclusive [arg-map]
  (op-expr arg-map `>=))

(defn max-exclusive [arg-map]
  (op-expr arg-map `<))

(defn min-exclusive [arg-map]
  (op-expr arg-map `>))

(defn string-literal [& chars]
  (let [s (read-string (apply str chars))]
   (if (number? s)
     s
     (str s))))

(def ast->clj-map  
  {
   :ident (fn[& chars] (read-string (apply str chars))) 
   :string-literal string-literal 
                                   
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




(def r 
  (clojure.core/fn
   [G__9464 G__9465]
   (clojure.core/if-let
    [base__9430__auto__
     (((:simpleTypes G__9464) ({:base "integer"} :base))
      G__9464
      G__9465)]
    (clojure.core/and
     base__9430__auto__
     (clojure.core/and
      ((clojure.core/fn
        [env__9237__auto__ value__9238__auto__]
        (clojure.core/<= value__9238__auto__ 10))
       G__9464
       G__9465)
      ((clojure.core/fn
        [env__9237__auto__ value__9238__auto__]
        (clojure.core/>= value__9238__auto__ 0))
       G__9464
       G__9465)))
    (throw (java.lang.IllegalArgumentException. "Unknown base")))))

