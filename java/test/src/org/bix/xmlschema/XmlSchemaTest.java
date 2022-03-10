package org.bix.xmlschema;

import static org.bix.xmlschema.XmlSchema.kwOf;
import static org.junit.Assert.assertEquals;

import java.util.Arrays;

import org.junit.Test;

public class XmlSchemaTest {
	@Test
	public void test_xml_schema() {
		final XmlSchema schema = XmlSchema.fromXmlLines(
		  "<schema xmlns:lib=\"myfile\">",
		  "<element name=\"elem\" type=\"positiveInteger\"/>",
		  "</schema>");
		assertEquals(Arrays.asList(kwOf("elem"), Arrays.asList(true, 10L)), 
					 schema.validateFromXmlString("<elem>10</elem>"));
		assertEquals(Arrays.asList(kwOf("elem"), Arrays.asList(false, -1L)), 
				 	 schema.validateFromXmlString("<elem>-1</elem>"));
	}
	
}
