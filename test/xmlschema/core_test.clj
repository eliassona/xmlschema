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
  #_(let [a (partial assert-schema p)]
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
    (is (= {:result true, :value "BMW"} (restriction env "BMW")))
    (is (= {:result false, :value "BMw"} (restriction env "BMw")))
    (is (= {:result true, :value "Audi"} (restriction env "Audi")))
    ))

(deftest ast-test-min-max
  #_(let [restriction (eval 
                       (ast->clj 
                           (parser 
                             (pr-str [:restriction {:base "integer"} 
                                      [:maxInclusive {:value "10"}]
                                      [:minInclusive {:value "0"}]]) 
                             :start :simpleType-restriction)))]
     (is (= {:result true, :value 5} (restriction env "5")))
     (is (= {:result true, :value 10} (restriction env "10")))
     (is (= {:result false, :value 11} (restriction env "11")))
     (is (= {:result true, :value "0"} (restriction env "0")))
     (is (= {:result false, :value -1} (restriction env "-1")))
     ))
