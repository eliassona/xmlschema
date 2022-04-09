# xmlschema

XML schema validator

## Usage


### Java
```java
import org.bix.xmlschema.XmlSchema;
final XmlSchema schema = XmlSchema.fromXmlLines(
  "<schema xmlns:lib=\"myfile\">",
  " <element name=\"elem1\" type=\"positiveInteger\"/>",
  " <element name=\"elem2\">",
  "  <simpleType>",
  "   <restriction base=\"string\">",
  "    <enumeration value=\"BMW\"/>",
  "    <enumeration value=\"Volvo\"/>",
  "   </restriction>",
  "  </simpleType>",
  " </element>",
  "</schema>");
assertXmlSchema(schema, "<elem1>10</elem1>", asList("elem1", 10L), true);
assertXmlSchema(schema, "<elem1>-1</elem1>", asList("elem1", -1L), false); 
assertXmlSchema(schema, "<elem2>BMW</elem2>", asList("elem2", "BMW"), true); 
assertXmlSchema(schema, "<elem2>Saab</elem2>", asList("elem2", "Saab"), false); 
assertEquals(new HashSet<>(
	asList(asList("elem1", "positiveInteger"),
	asList("elem2", "string"))), 
	schema.layout());

private void assertXmlSchema(final XmlSchema schema, final String inputXml, final List<Object> expectedResult, final boolean expectedValidity) {
	final Result r = schema.validateFromXmlString(inputXml);
	assertEquals(expectedValidity, r.isValid());
	assertEquals(expectedResult, r.getResult());
}

```

### Clojure

```clojure
(use 'xmlschema.core)
(let [schema (schema-compile 
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
    (let [result (schema [:a {:code "Pig", :base "hej"} [:b "elem"][:c "celem"]])]
      (assert (is-valid? result))
      (assert (= [:a {:code [true "Pig"], :base [true "hej"]} [true [:b [true "elem"]][:c [true "celem"]]]] result)))
    (let [result (schema [:a {:code "Dog", :base "hej"} [:b "elem"][:c "celem"]])]
      (assert (not (is-valid? result)))
      (assert (= [:a {:code [false "Dog"], :base [true "hej"]} [true [:b [true "elem"]][:c [true "celem"]]]] result))))

```


## License

Copyright © 2022 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
