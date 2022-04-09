package org.bix.xmlschema;

import static java.util.Arrays.asList;
import static org.bix.xmlschema.XmlSchema.kwOf;
import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.List;

import org.junit.Test;

public class XmlSchemaTest {
	@Test
	public void test_xml_schema() {
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
		assertEquals(asList("elem1", 10L), //with a better syntax ["elem1" 10]
					 schema.validateFromXmlString("<elem1>10</elem1>").getResult());
		assertEquals(asList("elem1", -1L),//with a better syntax ["elem1" -1] 
				 	 schema.validateFromXmlString("<elem1>-1</elem1>").getResult());
		assertEquals(asList("elem2", "BMW"),//with a better syntax ["elem2" "BMW"] 
			 	 schema.validateFromXmlString("<elem2>BMW</elem2>").getResult());
		assertEquals(asList("elem2", "Saab"),//with a better syntax ["elem2" "Saab"] 
			 	 schema.validateFromXmlString("<elem2>Saab</elem2>").getResult());
		assertEquals(new HashSet<>(
				            asList(asList("elem1", "positiveInteger"),
				            asList("elem2", "string"))), 
				schema.layout());//with a better syntax [["elem1" "positiveInteger"]["elem2" "string"]]
		
	}
	
	@Test
	public void test_parse() {
		String xml = XmlSchema.slurp("/Users/anderseliasson/src/xmlschema/resources/attribute_with_use.xml");
        List res = XmlSchema.parse(xml);
        
        
	}
}
