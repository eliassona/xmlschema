package org.bix.xmlschema;

import static clojure.java.api.Clojure.read;
import static clojure.java.api.Clojure.var;

import java.io.File;
import java.util.List;

import clojure.lang.IFn;

/**
 * XML schema implementation.
 * @author anderseliasson
 *
 */
public class XmlSchema {
	/**
	 * The result of a xml schema validation
	 * @author anderseliasson
	 *
	 */
	public static final class Result {
		private final List<Object> result;
		private final boolean isValidResult;

		private Result(final List<Object> r) {
			this.result = (List<Object>) makeJavaFriendly.invoke(r);
			this.isValidResult = (boolean)isValid.invoke(r);
		}
		/**
		 * Was the validation ok?
		 * @return
		 */
		public boolean isValid() {
			return isValidResult;
		}
		/**
		 * The data and status of the validation.
		 * @return
		 */
		public List<Object> getResult() {
			return result;
		}
		
	}
	
	private static final String CLOJURE_CORE = "clojure.core";
	private static final String XMLSCHEMA_CORE = "xmlschema.core";
	private final static IFn require;
	private final static IFn clSlurp;
	private final static IFn hiccupOf;
	private final static IFn layoutOf;
	private final static IFn keyword;
	private final static IFn parse;
	private final static IFn schemaToClj;
	private final static IFn schemaCompile;
	private final static IFn isValid;
	private final static IFn asXml;
	private final static IFn makeJavaFriendly;
	
	static {
		require = var(CLOJURE_CORE, "require");
		require.invoke(read(XMLSCHEMA_CORE));
		keyword = var(CLOJURE_CORE, "keyword");
		parse = var(XMLSCHEMA_CORE, "parse");
		clSlurp = var(CLOJURE_CORE, "slurp");
		schemaToClj = var(XMLSCHEMA_CORE, "schema->clj");
		schemaCompile = var(XMLSCHEMA_CORE, "schema-compile");
		isValid = var(XMLSCHEMA_CORE, "is-valid?");
		hiccupOf = var(XMLSCHEMA_CORE, "hiccup-of");
		layoutOf = var(XMLSCHEMA_CORE, "layout-of");
		asXml = var(XMLSCHEMA_CORE, "as-xml");
		makeJavaFriendly = var(XMLSCHEMA_CORE, "make-java-friendly");
		
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
	
	public static List<Object> parse(final String xml) {
		return (List<Object>) parse.invoke(xml);
	}
	
	public static Object schemaToClj(final String xml) {
		return schemaToClj.invoke(xml);
	}
	
	
	public static Object kwOf(final String str) {
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
	 * @return a result object
	 */
	public Result validateFromXmlString(final String xml) {
		return new Result((List<Object>) schema.invoke(hiccupOf.invoke(xml)));
	}
	

	/**
	 * Validate the xml data with this schema
	 * @param xmlLines the xml data as lines
	 * @return a result object
	 */
	public Result validateFromXmlLines(final String... xmlLines) {
		return validateFromXmlString(strOf(xmlLines));
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param filename the file of xml data
	 * @return a result object
	 */
	public Result validateFromXmlFile(final String filename) {
		return validateFromXmlString(slurp(filename));
	}
	
	/**
	 * Validate the xml data with this schema
	 * @param file the file of xml data
	 * @return a result object
	 */
	public Result validate(final File file) {
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
	
	private static String strOf(final String[] lines) {
		final StringBuilder buf = new StringBuilder();
		for (final String l: lines) {
			buf.append(l);
			buf.append("\n");
		}
		return buf.toString();
	}
}
