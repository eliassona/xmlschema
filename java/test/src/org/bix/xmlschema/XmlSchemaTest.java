package org.bix.xmlschema;

import static java.util.Arrays.asList;
import static org.junit.Assert.assertEquals;

import java.util.HashSet;
import java.util.List;

import org.bix.xmlschema.XmlSchema.Result;
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
		
		assertXmlSchema(schema, "<elem1>10</elem1>", asList("elem1", 10L), true);
		assertXmlSchema(schema, "<elem1>-1</elem1>", asList("elem1", -1L), false); 
		assertXmlSchema(schema, "<elem2>BMW</elem2>", asList("elem2", "BMW"), true); 
		assertXmlSchema(schema, "<elem2>Saab</elem2>", asList("elem2", "Saab"), false); 
		assertEquals(new HashSet<>(
				            asList(asList("elem1", "positiveInteger"),
				            asList("elem2", "string"))), 
				schema.layout());//with a better syntax [["elem1" "positiveInteger"]["elem2" "string"]]
		
	}
	
	private void assertXmlSchema(final XmlSchema schema, final String inputXml, final List<Object> expectedResult, final boolean expectedValidity) {
		final Result r = schema.validateFromXmlString(inputXml);
		assertEquals(expectedValidity, r.isValid());
		assertEquals(expectedResult, r.getResult());
	}

	@Test
	public void test_parse() {
		final String xml = XmlSchema.slurp("/Users/anderseliasson/src/xmlschema/resources/attribute_with_use.xml");
        final List res = XmlSchema.parse(xml);
        
        
	}
}
