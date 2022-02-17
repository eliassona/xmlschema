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


(defn schema-compile [hiccup start]
  (let [p (fn [text] (parser text :start start))]
    (-> hiccup pr-str p ast->clj eval))) 

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
  (let [restriction (schema-compile [:restriction {:base "string"} 
                              [:enumeration {:value "Audi"}]
                              [:enumeration {:value "BMW"}]] 
                            :simpleType-restriction)]
    (is (= [true "BMW"] (restriction env "BMW")))
    (is (= [false "BMw"] (restriction env "BMw")))
    (is (= [true "Audi"] (restriction env "Audi")))
    ))

(deftest ast-test-min-max
  (let [restriction (schema-compile [:restriction {:base "positiveInteger"} 
                                     [:maxInclusive {:value "10"}]] 
                            :simpleType-restriction)]
    (is (= [true 5] (restriction env "5")))
    ))


(deftest simple-type-element
  (let [schema 
        (schema-compile [:schema 
                         [:element {:name "car"} 
                          [:simpleType 
                            [:restriction {:base "integer"} 
                             [:maxInclusive {:value "10"}]
                             [:minInclusive {:value "0"}]]]]] :schema)]
    [:schema [:car "BMW"]]
    ))

(deftest arg-test
  (schema-compile [:annotation {:name "asdf", :public "asdf"}] :annotation)
  (schema-compile [:list {:itemType "string"}] :list)
  (schema-compile [:field {:xpath "asdf"}] :annotation)
  (schema-compile [:include {:schemaLocation "asdf"}] :annotation)
  )

