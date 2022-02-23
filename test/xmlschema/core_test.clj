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

(deftest test-empty-restriction
    (is (= [true "asdf"] ((schema-eval [:restriction {:base "string"}] :simpleType-restriction) env ["asdf"])))
    (is (= [true 10] ((schema-eval [:restriction {:base "integer"}] :simpleType-restriction) env ["10"])))
    (is (= [false -1] ((schema-eval [:restriction {:base "positiveInteger"}] :simpleType-restriction) env ["-1"])))
    )
    


(deftest test-simple-type
  (let [simpleType 
        (schema-eval [:simpleType 
                       [:restriction {:base "integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    (is (= [true 5] (simpleType env ["5"])))
    
    ))
(deftest test-named-simple-type
  (let [simpleType 
        (schema-eval [:simpleType {:name "car"} 
                       [:restriction {:base "integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    
    (is (= [true 5] (simpleType env ["5"])))
    
    ))

(deftest test-embedded-simple-type-element
  (let [element 
        (schema-eval [:element {:name "car"}
                      [:simpleType 
                        [:restriction {:base "integer"} 
                         [:maxInclusive {:value "10"}]
                         [:minInclusive {:value "0"}]]]] :element)]
    (is (= :car (-> element meta :name)))
    (is (= [:car [true 5]] (element env [:car "5"])))
    (is ( = :element (-> element meta :type)))
    ))

(deftest test-type-simple-type-element
  (let [schema 
        (schema-eval [:schema
                      [:simpleType {:name "hej"}
                         [:restriction {:base "integer"} 
                          [:maxInclusive {:value "10"}]
                          [:minInclusive {:value "0"}]]]
                      [:element {:name "car" :type "hej"}]] 
                     :schema)]
    (is (= [:car [true 5]] (schema [:car "5"])))
    ))


(deftest test-choice-type
  (let [type-fn
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
  (let [type-fn
        (schema-eval [:sequence {} 
                      [:element {:name "hej" :type "string"}]
                      [:element {:name "satoshi" :type "string"}]
                      [:sequence
                       [:element {:name "nakamoto" :type "string"}]
                       ]
                      ] :sequence)]
    (is (= [true [:hej [true "soffa"]][:satoshi [true "kudde"]]#_[:nakamoto [true "bla"]]] 
                  (type-fn env [[:hej "soffa"][:satoshi "kudde"]#_[:nakamoto "bla"]])))
    
    )
  )
(deftest test-all-type
  (let [type-fn
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
    (is (= [:car [true "asdf"]] (e env [:car "asdf"])))
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
    (is (= [:car [true 5]] (e env [:car "5"])))
    (is (= [:car [false 11]] (e env [:car "11"])))
    (is (= :element (-> e meta :type)))
    ))

(deftest test-schema-for-named-simple-type-element-at-schema-level
  (let [s 
        (schema-eval [:schema 
                      [:element {:name "car"}
                       [:simpleType 
                         [:restriction {:base "integer"} 
                          [:maxInclusive {:value "10"}]
                          [:minInclusive {:value "0"}]]]]] :schema)]
    (is (= [:car [true 5]] (s [:car "5"])))
    (is (= [:car [false 11]] (s [:car "11"])))
    ))

(deftest arg-test
  (schema-eval [:annotation {:name "asdf", :public "asdf"}] :annotation)
  (schema-eval [:list {:itemType "string"}] :list)
  (schema-eval [:field {:xpath "asdf"}] :annotation)
  (schema-eval [:include {:schemaLocation "asdf"}] :annotation)
  )


(deftest schema-layout
  (is (= {:elements [:a :b]
          :env 
          #{"string" "negativeInteger" 
            "short" "unsignedByte" 
            "a" "integer" "b" 
            "nonNegativeInteger" 
            "nonPositiveInteger" "unsignedShort" 
            "byte" "positiveInteger"}} 
         ((schema-eval [:schema 
                        [:element {:name "a" :type "string"}]
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

(deftest test-complexType
  (let [ct (schema-eval [:complexType
                                [:sequence 
                                 [:element {:name "a" :type "string"}]
                                 [:element {:name "b" :type "string"}]]
                                 ] :complexType)]
    
    (is (= [:a :b] (ct env)))
    (is (= [[true [:a [true "fiv"]] [:b [true "jiv"]]]] (ct env [{} [:a "fiv"][:b "jiv"]])))
  ))

(deftest test-complexType-embedded-element
  (let [ct (schema-eval [:element {:name "e"}
               [:complexType
                [:sequence 
                 [:element {:name "a" :type "string"}]
                 [:element {:name "b" :type "string"}]]
                ]] :element)]
    
    (is (= [:e [:a :b]] (ct env)))
    (is (= [:e [[true [:a [true "fiv"]] [:b [true "jiv"]]]]] (ct env [:e [:a "fiv"][:b "jiv"]])))
  ))

(deftest test-complexType-at-schema-level)


(deftest test-attribute-name 
  (let [a (schema-eval 
            [:attribute {:name "code"}
             [:simpleType
               [:restriction {:base "string"}
                 [:enumeration {:value "Pig"}]
                 [:enumeration {:value "Horse"}]]]]
            :attribute)]
    (is (= #{"code"} (a env)))
    (is (= [true "Pig"] (a env [:code "Pig"])))
    ))

(deftest test-attribute-name-at-schema-level 
  #_(let [s (schema-eval 
              [:schema
               [:element {:name "a"}
                [:complexType
                 [:attribute {:name "code"}
                  [:simpleType
                    [:restriction {:base "string"}
                      [:enumeration {:value "Pig"}]
                      [:enumeration {:value "Horse"}]]]]]
                ]
               ]
              :schema)]
     (is (= [true "Pig"] (s env [:a {:code "Pig"}])))
     ))
