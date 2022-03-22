(ns xmlschema.core-test
  (:require [clojure.test :refer :all]
            [xmlschema.core :refer :all]
            [instaparse.core :as insta])
  (:use [clojure.pprint]))


(defn assert-schema [resource start]
  (let [text (pr-str (hiccup-of (.trim (slurp (clojure.java.io/resource resource)))))
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
(deftest test-attribute-with-use
  (assert-schema "attribute_with_use.xml" :schema))
(deftest test-union-inline
  (assert-schema "union_inline.xml" :schema))

(deftest test-union-with-type
  (assert-schema "union_with_type.xml" :schema))
(deftest test-recursive
  (assert-schema "recursive.xml" :schema))

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

(deftest test-empty-restriction
    (is (= [true "asdf"] ((schema-eval [:restriction {:base "string"}] :simpleType-restriction) env "asdf")))
    (is (= [true 10] ((schema-eval [:restriction {:base "integer"}] :simpleType-restriction) env "10")))
    (is (= [false -1] ((schema-eval [:restriction {:base "positiveInteger"}] :simpleType-restriction) env "-1")))
    )
    

(deftest test-pattern-restriction
  (let [r (schema-eval [:restriction {:base "string"}
                        [:pattern {:value "[a-zA-Z][a-zA-Z][a-zA-Z]"}]] :simpleType-restriction)]
    (is (= [true "abc"] (r env "abc")))
    (is (= [false "abcd"] (r env "abcd")))))

(deftest test-simple-type
  (let [simpleType 
        (schema-eval [:simpleType 
                       [:restriction {:base "xs:integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    (is (= [true 5] (simpleType env "5")))
    
    ))
(deftest test-named-simple-type
  (let [simpleType 
        (schema-eval [:xs:simpleType {:name "car"} 
                       [:restriction {:base "integer"} 
                        [:maxInclusive {:value "10"}]
                        [:minInclusive {:value "0"}]]] :simpleType)]
    
    (is (= [true 5] (simpleType env "5")))
    
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
        (schema-compile [:schema {:xmlns:lib "myfile"}
                         [:simpleType {:name "hej"}
                            [:restriction {:base "integer"} 
                             [:maxInclusive {:value "10"}]
                             [:minInclusive {:value "0"}]]]
                         [:element {:name "car" :type "hej"}]])]
    (is (= [:car [true 5]] (schema [:car "5"])))
    ))


(deftest test-choice-type
  (let [type-fn
        (schema-eval [:choice {} 
                      [:element {:name "hej" :type "string"}]
                      [:element {:name "satoshi" :type "xs:string"}]
                      ] :choice)]
    (is (= [true [:hej [true "asdf"]]] (type-fn env [[:hej "asdf"]])))
    (is (= [false [:hej [true "asdf"]]] 
          (type-fn env [[:hej "asdf"][:hej "fsda"]])))
    (is (= [false [:hej [true "asdf"]]] 
          (type-fn env [[:hej "asdf"][:satoshi "fsda"]])))
    )
  )

(deftest test-sequence-type
  (let [s
        (schema-eval [:sequence {} 
                      [:element {:name "hej" :type "string"}]
                      [:element {:name "satoshi" :type "string"}]
                      [:sequence
                       [:element {:name "nakamoto" :type "string"}]
                       [:sequence
                        [:element {:name "oskar" :type "string"}]
                        ]
                       ]
                      ] :sequence)]
    (is (= [:hej :satoshi [:nakamoto :oskar]] (-> s meta :names)))
    (is (= [:hej :satoshi :nakamoto :oskar] (s env)))
    (is (= [true [:hej [true "soffa"]][:satoshi [true "kudde"]][:nakamoto [true "bla"]]] 
                 (s env [[:hej "soffa"][:satoshi "kudde"][:nakamoto "bla"]])))
    
    )
  )

(deftest test-complex-content-extension
  (let [type-fn
        (schema->clj 
          [:schema {:xmlns:person "myfile"}
           [:complexType {:name "personInfo"}
            [:sequence {} 
             [:element {:name "firstname" :type "string"}]
             [:element {:name "lastname" :type "string"}]]]
            
           [:complexType {:name "fullPersonInfo"}
            [:complexContent
             [:extension {:base "personInfo"}
              [:sequence {} 
               [:element {:name "address" :type "string"}]
               [:element {:name "city" :type "string"}]
               [:element {:name "country" :type "string"}]
               ]]]]
           [:element {:name "employee" :type "fullPersonInfo"}]] :schema)]
    type-fn
  ))

(deftest test-complex-referring-to-complexType
  (let [s
        (schema-compile 
          [:schema {:xmlns:person "myfile"}
           [:complexType {:name "personInfo"}
            [:sequence {} 
             [:element {:name "firstname" :type "string"}]
             [:element {:name "lastname" :type "string"}]]]
            
           [:complexType {:name "fullPersonInfo"}
            [:sequence {} 
             [:element {:name "info" :type "personInfo"}]
             [:element {:name "address" :type "string"}]
             [:element {:name "city" :type "string"}]
             [:element {:name "country" :type "string"}]
             ]]
           [:element {:name "employee" :type "fullPersonInfo"}]])]
    (is (= [:employee
            [true 
             [:info [true [:firstname [true "anders"]][:lastname [true "olson"]]]]
             [:address [true "solna"]]
             [:city [true "sthlm"]]
             [:country [true "swe"]]]]
           (s [:employee 
              [:info [:firstname "anders"][:lastname "olson"]] 
              [:address "solna"]
              [:city "sthlm"]
              [:country "swe"]])))
  ))

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
        (schema-compile [:schema {:xmlns:lib "myfile"}
                         [:element {:name "car"}
                          [:simpleType 
                            [:restriction {:base "integer"} 
                             [:maxInclusive {:value "10"}]
                             [:minInclusive {:value "0"}]]]]])]
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
  (is (= {:elements #{[:a "string"] [:b "string"]}
          :env 
          #{"date" "hexBinary" "decimal" 
            "base64Binary" "anyURI" 
            "string" "boolean" "negativeInteger" 
            "short" "unsignedByte" 
            "a" "integer" "b" 
            "nonNegativeInteger" 
            "nonPositiveInteger" "unsignedShort" 
            "byte" "positiveInteger"}} 
         ((schema-compile [:schema {:xmlns:lib "myfile"}
                           [:element {:name "a" :type "string"}]
                           [:element {:name "b" :type "string"}]]))))
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
    (is (= [[true [:a [true "fiv"]] [:b [true "jiv"]]]] (ct env [[:a "fiv"][:b "jiv"]])))
  ))

(deftest test-complexType-embedded-element
  (let [ct (schema-eval [:element {:name "e"}
               [:complexType
                [:sequence 
                 [:element {:name "a" :type "string"}]
                 [:element {:name "b" :type "string"}]]
                ]] :element)]
    
    (is (= [:e [:a :b]] (ct env)))
    (is (= [:e [true [:a [true "fiv"]] [:b [true "jiv"]]]] (ct env [:e [:a "fiv"][:b "jiv"]])))
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
    (is (= ["code" "string"] (a env)))
    (is (= {:code [true "Pig"]} (a env {:code "Pig"})))
    (is (= {:code [true nil]} (a env {})))
    ))

(deftest test-attribute-name-at-element-level 
  (let [s (schema-eval 
              [:element {:name "a"}
               [:complexType
                [:sequence
                 [:element {:name "b" :type "string"}]
                 [:element {:name "c" :type "string"}]]
                [:attribute {:name "code"}
                 [:simpleType
                   [:restriction {:base "string"}
                     [:enumeration {:value "Pig"}]
                     [:enumeration {:value "Horse"}]]]]
                [:attribute {:name "base" :type "string"}]
               ]]
             :element)]
    (is (= [:a {:code [true "Pig"], :base [true "hej"]} [true [:b [true "elem"]][:c [true "celem"]]]]
           (s env [:a {:code "Pig", :base "hej"} [:b "elem"][:c "celem"]])))
    ))

(deftest test-attribute-name-at-schema-level 
  (let [s (schema-compile 
              [:schema {:xmlns:lib "myfile"}
               [:element {:name "a"}
                [:complexType
                 [:sequence
                  [:element {:name "b" :type "string"}]
                  [:element {:name "c" :type "string"}]]
                 [:attribute {:name "code"}
                  [:simpleType
                    [:restriction {:base "string"}
                      [:enumeration {:value "Pig"}]
                      [:enumeration {:value "Horse"}]]]]
                 [:attribute {:name "base" :type "string"}]
                ]]])]
    (let [result (s [:a {:code "Pig", :base "hej"} [:b "elem"][:c "celem"]])]
      (is (= true (-> result second meta :result)))
      (is (= [:a {:code [true "Pig"], :base [true "hej"]} [true [:b [true "elem"]][:c [true "celem"]]]]
             result)))
    ))

(deftest test-boolan-element
  (let [e (schema-eval [:element {:name "a" :type "xs:boolean"}] :element)]
    (is (= [:a [true true]] (e env [:e "true"])))
    (is (= [:a [false "hej"]] (e env [:e "hej"])))))
    
(deftest test-include
  (let [s (schema-compile [:schema {:xmlns:lib "myfile"} [:include {:schemaLocation "typed_elements.xml"}]
                          [:element {:type "string", :name "a"}]])]
    (is (= {:env #{"boolean" "string" "hexBinary" "student" "member" "club" "club1"
                    "negativeInteger" "short" "unsignedByte" 
                    "employee" "a" "integer" "nonNegativeInteger" 
                    "anyURI" "nonPositiveInteger" "date" 
                    "base64Binary" "unsignedShort" "byte" 
                    "positiveInteger" "decimal"}, 
            :elements #{[:member "string"] [:student "string"] [:employee "string"] [:a "string"]}} 
            (s)))
    (is (= [:student [true "hej"]] (s [:student "hej"])))
  ))



(deftest test-positiveInteger
  (let [e (schema-eval [:element {:name "a" :type "positiveInteger"}] :element)]
    (is (= [:a [true 1]] (e env [:a "1"])))
  ))

(deftest test-union
  (let [u (schema-eval [:union {:memberTypes "xs:positiveInteger string"}] :union)]
    (is (= [true 1] (u env "1")))
    (is (= [true "-1"] (u env "-1")))
    ))
    
(deftest test-import 
  (let [s (schema-eval [:schema {:xmlns:hej "adsf"} 
                      [:import {:schemaLocation "typed_elements.xml"}]
                      [:element {:name "a" :type "lib:club"}]] :schema)]
    (is (= [:a [true 1]] (s [:a "1"])))
    (is (= [:a [false 0]] (s [:a "0"])))
    (is (= [:a [false 10000]] (s [:a "10000"])))
    (is (= [:a [false 9999]] (s [:a "9999"])))
    (is (= [:a [true 99]] (s [:a "99"])))
    
    ))

(deftest test-simple-extension
  (let [e (schema-eval [:extension {:base "integer"}] :simpleContent-extension)]
    (is (= [{} [true 10]] (e env [{} "10"])))
  (let [e (schema-eval [:extension {:base "integer"}
                        [:attribute {:name "attr1", :type "integer"}]
                        [:attribute {:name "attr2", :type "integer"}]] :simpleContent-extension)]
    (is (= [{:attr1 [true 10], :attr2 [true 20]}  [true 11]] (e env [{:attr1  "10", :attr2 "20"} "11"]))))
  ))

(deftest test-simple_content
  (let [e (schema-eval [:simpleContent [:extension {:base "integer"}]] :simpleContent)]
    (is (= (-> e meta :type) :simpleContent))
    (is (= [{} [true 10]] (e env [{} "10"])))
  (let [e (schema-eval [:simpleContent 
                        [:extension {:base "integer"}
                         [:attribute {:name "attr1", :type "integer"}]
                         [:attribute {:name "attr2", :type "integer"}]]] :simpleContent)]
    (is (= [{:attr1 [true 10], :attr2 [true 20]}  [true 11]] (e env [{:attr1  "10", :attr2 "20"} "11"]))))
  ))

(deftest test-simple-content_from-element
  (let [e (schema-eval [:element {:name "a"} 
                        [:complexType [:simpleContent [:extension {:base "integer"}]]]] :element)]
    (is (= [:a {} [true 10]] (e env [:a {} "10"])))
  (let [e (schema-eval [:element {:name "a"}
                        [:complexType 
                         [:simpleContent 
                          [:extension {:base "integer"}
                          [:attribute {:name "attr1", :type "integer"}]
                          [:attribute {:name "attr2", :type "integer"}]]]]] :element)]
    (is (= [:a {:attr1 [true 10], :attr2 [true 20]}  [true 11]] 
           (e env [:a {:attr1  "10", :attr2 "20"} "11"]))))
  ))

(deftest test-list
  (let [e (schema-eval [:schema {:xmlns:hej "adsf"}
                        [:element {:name "intvalues"}
                         [:simpleType
                          [:list {:itemType "positiveInteger"}]]]] :schema)]
    (is (= [:intvalues [true [true 1] [true 2] [true 3]]] (e [:intvalues "1 2 3"])))
    (is (= [:intvalues [true [true 1] [true 2] [false "3.0"]]] (e [:intvalues "1 2 3.0"])))
    (is (= [:intvalues [true [true 1] [false -2] [false "3.0"]]] (e [:intvalues "1 -2 3.0"])))
  ))


(deftest test-element-ref
  (let [r (schema-compile 
            [:schema {:xmlns:hej "adsf"}
             [:element {:name "intvalues" :type "string"}]])
        ref-env (-> r meta :env)
        e (schema-eval [:element {:ref "intvalues"}] :element)]
    (is (= [:intvalues [true "1 2 3"]] 
           (e ref-env [:invalues "1 2 3"])))
    (is (= [:intvalues "string"] (e ref-env)))
    
    ))

(deftest test-named-group
  (let [s (schema-compile 
            [:schema {:xmlns:hej "adsf"}
             [:group {:name "g"}
              [:all
               [:element {:name "a" :type "string"}]
              ]]])
        env (-> s meta :env)
        g (env "g")]
    (is (= :group (-> g meta :type)))
    (is (= [:a] (g env)))
    (is (= [true [:a [true "hej"]]] (g env [[:a "hej"]])))
  ))

(deftest test-attributeGroup
  (let [s (schema-compile 
            [:schema {:xmlns:hej "adsf"}
             [:attributeGroup {:name "personattr"}
              [:attribute {:name "attr1" :type "string"}]
              [:attribute {:name "attr2" :type "integer"}]]
             [:complexType {:name "person"}
              [:attributeGroup {:ref "personattr"}]]
             [:element {:name "olle" :type "person"}]])]
    (is (= [:olle {:attr1 [true "a1"], :attr2 [false "a2"]} []] (s [:olle {:attr1 "a1" :attr2 "a2"}])))
    ))
        
(deftest test-big-one
  (let [s (schema-compile
         [:schema {:xmlns:hej "adsf"}
          [:import {:schemaLocation "bigone.xml"}]
          [:element {:name "a" :type "lib:AccountIdentification4Choice_HR2"}]
          ])]
   (is (= [:a [false [:IBAN [false "hej"]]]] (s [:a [:IBAN "hej"]])))
   ))


(deftest test-sequence-when-referring-to-new-type
  (let [s (schema-compile
         [:schema {:xmlns:hej "adsf"}
          [:complexType {:name "AccountIdentification4Choice_HR2"}
           [:sequence
            [:element {:name "IBAN" :type "IBAN2007Identifier"}]
            ]]
          [:simpleType {:name "IBAN2007Identifier"}
           [:restriction {:base "xs:string"}
            [:enumeration {:value "A"}]
            [:enumeration {:value "B"}]
            ]]
          [:element {:name "a" :type "AccountIdentification4Choice_HR2"}]])]
    (is (= [:a [false [:IBAN [false "hej"]]]] (s [:a [:IBAN "hej"]])))
    (is (= [:a [true [:IBAN [true "A"]]]] (s [:a [:IBAN "A"]])))
    (is (= [:a [true [:IBAN [true "B"]]]] (s [:a [:IBAN "B"]])))
    ))


(deftest test-trying-to-override-from-other-ns
  (is (thrown? IllegalArgumentException
        (schema-compile
         [:schema {:xmlns:hej "adsf"}
          [:element {:name "lib:a" :type "string"}]
          [:element {:name "hej" :type "string"}]]))))


(deftest test-redefine
  #_(let [s (schema-compile
          [:schema {:xmlns:hej "adsf"}
           [:import {:schemaLocation "typed_elements.xml"}]
           [:element {:name "a" :type "lib:AccountIdentification4Choice_HR2"}]
           ])]
    (is (= [] (s [:a [:IBAN "hej"]])))
    )
  )
  
(deftest test-normalize-coll-data
  (is (= [] (normalize-coll-data [] []))) 
  (is (= [[:a 1][:b 2][:c 3][:d 4]] (normalize-coll-data [[:a 1][:b 2][:c 3][:d 4]] []))) 
  (is (= [[:a 1][[:b 2][:c 3]][:d 4]] (normalize-coll-data [[:a 1][:b 2][:c 3][:d 4]] [:a [:b :c] :d])))
  (is (= [[:a 1][[:b 2]]] (normalize-coll-data [[:a 1][:b 2]] [:a [:b :c]])))
  (is (= [[:a 1][[:b 2]][:c 3][:d 4]] (normalize-coll-data [[:a 1][:b 2][:c 3][:d 4]] [:a [:b] :d])))
)  