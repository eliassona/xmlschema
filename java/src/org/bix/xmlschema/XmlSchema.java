package org.bix.xmlschema;

import java.io.File;
import java.util.List;
import java.util.Map;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

/**
 * XML schema implementation.
 * @author anderseliasson
 *
 */
public class XmlSchema {
	
	private static final String CLOJURE_CORE = "clojure.core";
	private static final String XMLSCHEMA_CORE = "xmlschema.core";
	private final static IFn require;
	private final static IFn schemaEval;
	private final static IFn clSlurp;
	private final static IFn hiccupOf;
	private final static IFn layoutOf;
	private final static IFn keyword;
	private final static IFn parse;
	
	static {
		require = Clojure.var(CLOJURE_CORE, "require");
		require.invoke(Clojure.read(XMLSCHEMA_CORE));
		keyword = Clojure.var(CLOJURE_CORE, "keyword");
		parse = Clojure.var(XMLSCHEMA_CORE, "parse");
		clSlurp = Clojure.var(CLOJURE_CORE, "slurp");
		schemaEval = Clojure.var(XMLSCHEMA_CORE, "schema-eval");
		hiccupOf = Clojure.var(XMLSCHEMA_CORE, "hiccup-of");
		layoutOf = Clojure.var(XMLSCHEMA_CORE, "layout-of");
	}
	
	/**
	 * Create an xml schema object from the xml schema string
	 * @param xml the xml schema string
	 * @return
	 */
	public static final XmlSchema fromXmlString(final String xml) {
		return new XmlSchema(xml);
	}
	/**
	 * Create an xml schema object from the xml schema string
	 * @param xmlLines the xml schema as lines of xml strings
	 * @return
	 */
	public static final XmlSchema fromXmlLines(final String... xmlLines) {
		return fromXmlString(strOf(xmlLines));
	}
	/**
	 * Create an xml schema object from file
	 * @param filename
	 * @return
	 */
	public static final XmlSchema fromXmlFile(final String filename) {
		return fromXmlString(slurp(filename));
	}
	
	public static List parse(String xml) {
		return (List) parse.invoke(xml);
	}
	
	public static Object kwOf(String str) {
		return keyword.invoke(str);
	}
	public static final String slurp(final Object filenameOrString) {
		return (String) clSlurp.invoke(filenameOrString);
	}
	private final IFn schema;

	private XmlSchema(final String xml) {
		schema = (IFn) schemaEval.invoke(xml);
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param xml the xml data
	 * @return a list containing the result of the validation
	 */
	public List validateFromXmlString(String xml) {
		return (List) schema.invoke(hiccupOf.invoke(xml));
	}

	/**
	 * Validate the xml data with this schema
	 * @param xmlLines the xml data as lines
	 * @return a list containing the result of the validation
	 */
	public List validateFromXmlLines(String... xmlLines) {
		return validateFromXmlString(strOf(xmlLines));
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param filename the file of xml data
	 * @return a list containing the result of the validation
	 */
	public List validateFromXmlFile(String filename) {
		return validateFromXmlString(slurp(filename));
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param file the file of xml data
	 * @return a list containing the result of the validation
	 */
	public List validate(File file) {
		return validateFromXmlString(slurp(file));
	}
	
	/**
	 * 
	 * @return the layout of the schema as a list
	 */
	public List layout() {
		return (List) layoutOf.invoke(schema);
	}
	
	@Override
	public String toString() {
		return schema.invoke().toString();
	}
	
	private static String strOf(String[] lines) {
		StringBuilder buf = new StringBuilder();
		for (String l: lines) {
			buf.append(l);
			buf.append("\n");
		}
		return buf.toString();
	}

	
	public static void main(final String[] args) {
		final XmlSchema s = XmlSchema.fromXmlFile("/Users/anderseliasson/src/xmlschema/resources/typed_elements.xml");
		System.out.println(s.toString());
		System.out.println(s.validateFromXmlString("<employee>hej</employee>"));
		System.out.println(s.layout());
	}
}
