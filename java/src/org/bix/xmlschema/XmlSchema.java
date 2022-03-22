package org.bix.xmlschema;

import java.io.File;
import java.util.List;

import clojure.java.api.Clojure;
import clojure.lang.IFn;

/**
 * XML schema implementation.
 * @author anderseliasson
 *
 */
public class XmlSchema {
	public static final class Result {
		private final List<Object> result;

		private Result(List<Object> r) {
			this.result = r;
		}
		public boolean isValid() {
			return (boolean) isValid.invoke(result);
		}
		public List<Object> getResult() {
			return result;
		}
	}
	
	private static final String CLOJURE_CORE = "clojure.core";
	private static final String XMLSCHEMA_CORE = "xmlschema.core";
	private final static IFn require;
	private final static IFn schemaEval;
	private final static IFn clSlurp;
	private final static IFn hiccupOf;
	private final static IFn layoutOf;
	private final static IFn keyword;
	private final static IFn parse;
	private final static IFn schemaToClj;
	private final static IFn schemaCompile;
	private final static IFn isValid;
	
	static {
		require = Clojure.var(CLOJURE_CORE, "require");
		require.invoke(Clojure.read(XMLSCHEMA_CORE));
		keyword = Clojure.var(CLOJURE_CORE, "keyword");
		parse = Clojure.var(XMLSCHEMA_CORE, "parse");
		clSlurp = Clojure.var(CLOJURE_CORE, "slurp");
		schemaEval = Clojure.var(XMLSCHEMA_CORE, "schema-eval");
		schemaToClj = Clojure.var(XMLSCHEMA_CORE, "schema->clj");
		schemaCompile = Clojure.var(XMLSCHEMA_CORE, "schema-compile");
		isValid = Clojure.var(XMLSCHEMA_CORE, "is-valid?");
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
	
	public static List<Object> parse(String xml) {
		return (List<Object>) parse.invoke(xml);
	}
	
	public static Object schemaToClj(String xml) {
		return schemaToClj.invoke(xml);
	}
	
	
	public static Object kwOf(String str) {
		return keyword.invoke(str);
	}
	public static final String slurp(final Object filenameOrString) {
		return (String) clSlurp.invoke(filenameOrString);
	}
	private final IFn schema;

	private XmlSchema(final String xml) {
		schema = (IFn) schemaCompile.invoke(xml);
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param xml the xml data
	 * @return a list containing the result of the validation
	 */
	public Result validateFromXmlString(String xml) {
		return new Result((List<Object>) schema.invoke(hiccupOf.invoke(xml)));
	}
	

	/**
	 * Validate the xml data with this schema
	 * @param xmlLines the xml data as lines
	 * @return a list containing the result of the validation
	 */
	public Result validateFromXmlLines(String... xmlLines) {
		return validateFromXmlString(strOf(xmlLines));
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param filename the file of xml data
	 * @return a list containing the result of the validation
	 */
	public Result validateFromXmlFile(String filename) {
		return validateFromXmlString(slurp(filename));
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param file the file of xml data
	 * @return a list containing the result of the validation
	 */
	public Result validate(File file) {
		return validateFromXmlString(slurp(file));
	}
	
	/**
	 * 
	 * @return the layout of the schema as a list
	 */
	public List<Object> layout() {
		return (List<Object>) layoutOf.invoke(schema);
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
