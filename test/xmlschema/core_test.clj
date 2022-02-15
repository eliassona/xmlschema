(ns xmlschema.core-test
  (:require [clojure.test :refer :all]
            [xmlschema.core :refer :all]
            [instaparse.core :as insta])
  (:use [clojure.pprint]))


(defn assert-schema [p resource start]
  (let [text (pr-str (hiccup-of (slurp (clojure.java.io/resource resource))))
        result (p text start parser)]
    (if (insta/failure? result)
      (is false (insta/get-failure result))
      (is true))))

(defn assert-schemas [p]
  (let [a (partial assert-schema p)]
	  (a "typed_element.xml" :schema)
	  (a "typed_elements.xml" :schema)
	  (a "element_with_embedded_restrictions.xml" :schema)
	  (a "predef_restrictions.xml" :schema)
	  (a "complex_type.xml" :schema)
	  (a "group.xml" :schema)
	  (a "choice.xml" :schema)
   )
)  

(deftest parse-test
  (assert-schemas parser)
  )
  
(deftest ast-test-enum
  (let [restriction (eval 
                      (ast->clj 
                          (parser 
                            (pr-str [:restriction {:base "string"} 
                                     [:enumeration {:value "Audi"}]
                                     [:enumeration {:value "BMW"}]]) 
                            :start :simpleType-restriction)))]
    (is (= [true "BMW"] (restriction env "BMW")))
    (is (= [false "BMw"] (restriction env "BMw")))
    (is (= [true "Audi"] (restriction env "Audi")))
    ))

(deftest ast-test-min-max
  (let [restriction (eval 
                      (ast->clj 
                          (parser 
                            (pr-str [:restriction {:base "integer"} 
                                     [:maxInclusive {:value "10"}]
                                     [:minInclusive {:value "0"}]]) 
                            :start :simpleType-restriction)))]
    (is (= [true 5] (restriction env "5")))
    (is (= [true 10] (restriction env "10")))
    (is (= [false 11] (restriction env "11")))
    (is (= [true 0] (restriction env "0")))
    (is (= [false -1] (restriction env "-1")))
    ))


(deftest simple-type-element
  (let [schema 
        (eval 
          (ast->clj 
              (parser 
                (pr-str [:schema 
                         [:element {:name "car"} 
                          [:simpleType 
                            [:restriction {:base "integer"} 
                             [:maxInclusive {:value "10"}]
                             [:minInclusive {:value "0"}]]]]]))))]
    [:schema [:car "BMW"]]
    ))