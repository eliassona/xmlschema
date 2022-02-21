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
    (is (= [true "BMW"] (restriction env ["BMW"])))
    (is (= [false "BMw"] (restriction env ["BMw"])))
    (is (= [true "Audi"] (restriction env ["Audi"])))
    ))

(deftest ast-test-min-max
  (let [restriction (schema-eval [:restriction {:base "positiveInteger"} 
                                     [:maxInclusive {:value "10"}]] 
                            :simpleType-restriction)]
    (is (= [true 5] (restriction env ["5"])))
    ))


(deftest test-simple-type
  (let [simpleType 
        (schema-eval [:simpleType 
                       [:restriction {:base "integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    (is [true 5] (simpleType env ["5"]))
    
    ))
(deftest test-named-simple-type
  (let [[name type-fn] 
        (schema-eval [:simpleType {:name "car"} 
                       [:restriction {:base "integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    
    (is (= "car" name))
    (is [true 5] (type-fn env ["5"]))
    
    ))

(deftest test-simple-type-element
  (let [element 
        (schema-eval [:element {:name "car"}
                      [:simpleType 
                        [:restriction {:base "integer"} 
                         [:maxInclusive {:value "10"}]
                         [:minInclusive {:value "0"}]]]] :element)]
    (is (= :car (-> element meta :name)))
    (is [true 5] (element env ["5"]))
    (is :element (-> element meta :type))
    ))

(deftest test-choice-type
  #_(let [type-fn
         (schema-eval [:choice {} 
                       [:element {:name "hej" :type "string"}]
                       [:element {:name "satoshi" :type "string"}]
                       ] :choice)]
     (is (= [true [:hej [true "asdf"]]] (type-fn env [[:hej "asdf"]])))
     (is (= [false [:hej [true "asdf"]][:hej [true "fsda"]]] 
           (type-fn env [[:hej "asdf"][:hej "fsda"]])))
     (is (= [false [:hej [true "asdf"]][:satoshi [true "fsda"]]] 
           (type-fn env [[:hej "asdf"][:satoshi "fsda"]])))
     )
  )





(deftest test-sequence-type
  #_(let [type-fn
         (schema-eval [:sequence {} 
                       [:element {:name "hej" :type "string"}]
                       [:element {:name "satoshi" :type "string"}]
                       [:sequence
                        [:element {:name "nakamoto" :type "string"}]
                        ]
                       ] :sequence)]
     (is (= [true [:hej [true "soffa"]][:satoshi [true "kudde"]]] 
                   (type-fn env [[:hej "soffa"][:satoshi "kudde"]])))
    
     )
  )
(deftest test-all-type
  #_(let [type-fn
         (schema-eval [:all {} 
                       [:element {:name "hej" :type "string"}]
                       [:element {:name "satoshi" :type "string"}]
                       ] :all)]
     (is (= [true [:hej [true "soffa"]][:satoshi [true "kudde"]]] 
                   (type-fn env [[:hej "soffa"][:satoshi "kudde"]])))
    
     )
  )

(deftest test-element-with-type-arg
  (let [e (schema-eval [:element {:name "car", :type "string"}] :element)]
    (is (= :car (-> e meta :name)))
    (is (= [true "asdf"] (e env ["asdf"])))
    (is (= :element (-> e meta :type)))
  ))
               

(deftest test-schema-for-named-simple-type-element
  (let [e 
        (schema-eval [:element {:name "car"}
                      [:simpleType 
                        [:restriction {:base "integer"} 
                         [:maxInclusive {:value "10"}]
                         [:minInclusive {:value "0"}]]]] :element)]
    (is (= :car (-> e meta :name)))
    (is [true 5] (e env ["5"]))
    (is [false 11] (e env ["11"]))
    (is (= :element (-> e meta :type)))
    ))

(deftest test-schema-for-named-simple-type-element-at-schema-level
  #_(let [s 
         (schema-eval [:schema 
                       [:element {:name "car"}
                        [:simpleType 
                          [:restriction {:base "integer"} 
                           [:maxInclusive {:value "10"}]
                           [:minInclusive {:value "0"}]]]]] :schema)]
     (is [true 5] (e env [:car "5"]))
     (is [false 11] (e env [:car "11"]))
     ))

(deftest arg-test
  (schema-eval [:annotation {:name "asdf", :public "asdf"}] :annotation)
  (schema-eval [:list {:itemType "string"}] :list)
  (schema-eval [:field {:xpath "asdf"}] :annotation)
  (schema-eval [:include {:schemaLocation "asdf"}] :annotation)
  )


(deftest schema-layout
  (is (= [:a :b] ((schema-eval [:schema [:element {:name "a" :type "string"}]
                              [:element {:name "b" :type "string"}]] :schema))))
  #_(is (= [:hej :satoshi :nakamoto :bitcoin :a] 
          ((schema-eval [:sequence {} 
                        [:element {:name "hej" :type "string"}]
                        [:element {:name "satoshi" :type "string"}]
                        [:sequence
                         [:element {:name "nakamoto" :type "string"}]
                         [:element {:name "bitcoin" :type "string"}]
                         [:choice
                          [:element {:name "a" :type "string"}]
                          ]
                         ]
                        ] :sequence))))
  
  )


