(ns xmlschema.core-test
  (:require [clojure.test :refer :all]
            [xmlschema.core :refer :all]
            [instaparse.core :as insta])
  (:use [clojure.pprint]))


(defn assert-schema [resource start]
  (let [text (pr-str (hiccup-of (slurp (clojure.java.io/resource resource))))
        result (parser text :start start)]
    (if (insta/failure? result)
      (is false (insta/get-failure result))
      (do 
        #_(ast->clj result)
        (is true)))))

 



(deftest parse-test 
  (let [a (partial assert-schema)]
	  ;(a "typed_element.xml" :schema)
	  ;(a "typed_elements.xml" :schema)
	  ;(a "element_with_embedded_restrictions.xml" :schema)
	  ;(a "group.xml" :schema)
	  
	 ; (a "complex_type.xml" :schema)
;	  (a "totaldigits.xml" :schema)
  ))
(deftest test-extension 
  (assert-schema "extension.xml" :element))
(deftest test-group
  (assert-schema "group.xml" :schema))
(deftest test-choice
  (assert-schema "choice.xml" :schema))
(deftest test-predefs
  (assert-schema "predef_restrictions.xml" :schema))
(deftest test-keyref
  (assert-schema "keyref.xml" :keyref))

(deftest ast-test-enum
  (let [restriction (schema-eval [:restriction {:base "string"} 
                              [:enumeration {:value "Audi"}]
                              [:enumeration {:value "BMW"}]] 
                            :simpleType-restriction)]
    (is (= [true "BMW"] (restriction env "BMW")))
    (is (= [false "BMw"] (restriction env "BMw")))
    (is (= [true "Audi"] (restriction env "Audi")))
    ))

(deftest ast-test-min-max
  (let [restriction (schema-eval [:restriction {:base "positiveInteger"} 
                                     [:maxInclusive {:value "10"}]] 
                            :simpleType-restriction)]
    (is (= [true 5] (restriction env "5")))
    ))


(deftest test-simple-type
  (let [simpleType 
        (schema-eval [:simpleType 
                       [:restriction {:base "integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    (is [true 5] (simpleType env "5"))
    
    ))
(deftest test-named-simple-type
  (let [[name type-fn] 
        (schema-eval [:simpleType {:name "car"} 
                       [:restriction {:base "integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    
    (is (= "car" name))
    (is [true 5] (type-fn env "5"))
    
    ))

(deftest test-simple-type-element
  (let [[name type-fn type] 
        (schema-eval [:element {:name "car"}
                      [:simpleType 
                        [:restriction {:base "integer"} 
                         [:maxInclusive {:value "10"}]
                         [:minInclusive {:value "0"}]]]] :element)]
    (is (= "car" name))
    (is [true 5] (type-fn env "5"))
    (is :element type)
    ))

(deftest test-complex-type
  (schema-eval [:complexType 
                 [:sequence  
                  [:element {:name "hej" :type "string"}]
                  [:element {:name "satoshi" :type "string"}]
                  ]] :complexType)
  )

(deftest test-element-with-type-arg
  (let [[name type-fn type]
        (schema-eval [:element {:name "car", :type "string"}] :element)]
    (is (= "car" name))
    (is (= [true "asdf"] (type-fn env "asdf")))
    (is (= :element type))
  ))
               

(deftest test-schema-for-named-simple-type-element
  #_(let [[name type-fn type] 
         (schema-eval [:schema 
                       [:element {:name "car"}
                        [:simpleType 
                          [:restriction {:base "integer"} 
                           [:maxInclusive {:value "10"}]
                           [:minInclusive {:value "0"}]]]]] :element)]
     (is (= "car" name))
     (is [true 5] (type-fn env "5"))
     (is :element type)
     ))

(deftest arg-test
  (schema-eval [:annotation {:name "asdf", :public "asdf"}] :annotation)
  (schema-eval [:list {:itemType "string"}] :list)
  (schema-eval [:field {:xpath "asdf"}] :annotation)
  (schema-eval [:include {:schemaLocation "asdf"}] :annotation)
  )



