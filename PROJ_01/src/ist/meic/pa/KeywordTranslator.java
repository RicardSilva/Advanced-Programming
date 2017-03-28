/*
KeywordTranslator: Search for KeywordArgs annotations and injects code where needed
to initialize class fields with their default values and build constructors that accept
keywords as arguments
*/


package ist.meic.pa;

import javassist.*;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class KeywordTranslator implements Translator {
	
	/////////////////////////////
	///// Auxiliary Methods /////
	/////////////////////////////
	
	// Returns constructor with params (Object... args)
	private CtConstructor getSpecialConstructor(CtClass ctClass) throws NotFoundException {
		CtClass objectArr = ClassPool.getDefault().getCtClass(Object[].class.getName());
		CtClass[] params = {objectArr};
		return ctClass.getDeclaredConstructor(params);
	}
	
	// Parse @KeywordArgs parameters and default values and return them as a map
	private Map<String,String> parseKeywordArgs(KeywordArgs annotation) {
		Map<String,String> keywordParams = new HashMap<String,String>();
		
		for(String paramString : annotation.value().split(",")) {
            String[] pair = paramString.split("=");
            String paramName = pair[0].trim();
			String paramValue = null;
			if(pair.length == 2) {
				paramValue = pair[1].trim();
			}
            keywordParams.put(paramName, paramValue);
		}
        return keywordParams;
	}
	
	// Return a set containing all params that were defined in a class (and superclasses) using @KeywordArgs
	private Set<String> findAllKeywordParams(CtClass ctClass) throws NotFoundException, ClassNotFoundException {
		CtClass currentClass = ctClass;
		Set<String> keywordParams = new HashSet<String>();
		
		while(currentClass != null) {
			try {
				CtConstructor ctConst = getSpecialConstructor(currentClass);
				Object annotation = ctConst.getAnnotation(KeywordArgs.class);
				if(annotation != null) {
					keywordParams.addAll(parseKeywordArgs((KeywordArgs) annotation).keySet());
				}
			}
			catch (NotFoundException ex) {
				// jump straight to parent class
			}
			
			currentClass = currentClass.getSuperclass();
		}
		return keywordParams;
	}

	//////////////////////////////////
	///// Code-Injection Methods /////
	//////////////////////////////////

	// Inject method to check whether a class (or superclasses) use a given field as a @KeywordArgs parameter
	private void InjectContainsKeywordMethod(CtClass ctClass)
			throws NotFoundException, ClassNotFoundException, CannotCompileException {
		String code = "static boolean containsKeyword(String keyword) { return ";
		
		for(String keywordParam : findAllKeywordParams(ctClass)){
			code += "keyword.equals(\"" + keywordParam + "\") || ";
		}
		code += "false; }";

		CtMethod method = CtMethod.make(code, ctClass);
		ctClass.addMethod(method);
	}

	// Inject code to update fields to the values that were passed as arguments
	private void InjectUpdateFieldsCode(CtConstructor ctConst) throws CannotCompileException {
		String code = "";
		
		code += "for (int i = 0; i < $1.length / 2 ; i++)	{";
		code += "	String fieldName = (String) $1[2*i];";
		code += "	if(!containsKeyword(fieldName))";
		code += "		throw new RuntimeException(\"Unrecognized keyword: \" + fieldName);";
		code += "   Object value = $1[2*i+1];";
		code += "   boolean fieldFound = false;";
		code += "   Class current = this.getClass();";
		code += "   while(current.getSuperclass() != null && !fieldFound) {";
		code += "		try {";
		code += "			java.lang.reflect.Field field = current.getDeclaredField(fieldName);";
		code += "           fieldFound = true;";
		code += "			field.setAccessible(true);";
		code += "			field.set(this, value);";
		code += "		} catch (NoSuchFieldException e) {";
		code += "			current = current.getSuperclass();";
		code += "  		}";
		code += "   }";
		code += "   if (!fieldFound) {";
		code += "		throw new IllegalArgumentException(\"The argument was not defined in the constructor.\");";
		code += "	}";
		code += "}";
		
		ctConst.insertBeforeBody(code);
	}

	// Inject code to initialize fields present in @KeywordArgs to their default values
	private void InjectFieldInitializers(CtConstructor ctConst, KeywordArgs annotation) throws CannotCompileException {
		Map<String,String> arguments = parseKeywordArgs(annotation);
		
		String code = "";
		for(String arg : arguments.keySet()){
			if(arguments.get(arg) != null) // let java deal with uninitialiazed fields
				code += arg + "=" + arguments.get(arg) + ";";
        }
        
		ctConst.insertBeforeBody(code);
	}

	///////////////////////////
	///// Default Methods /////
	///////////////////////////

	public void start(ClassPool pool) throws NotFoundException, CannotCompileException {
		// Do nothing
	}
	
	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
		CtClass ctClass = pool.getCtClass(className);
		CtConstructor ctConst;
		
		// Get constructor
		try {
			ctConst =  getSpecialConstructor(ctClass);
		}
		catch (NotFoundException ex) {
			return; // The constructor doesn't exist
		}
			
		// If @KeywordArgs annotation is present modify constructor
		try {
			KeywordArgs annotation = (KeywordArgs) ctConst.getAnnotation(KeywordArgs.class);
			if(annotation != null) {
				InjectContainsKeywordMethod(ctClass);
				InjectUpdateFieldsCode(ctConst);
				InjectFieldInitializers(ctConst, annotation);
	        }
		} catch (ClassNotFoundException ex) {
			ex.printStackTrace(); // Thrown by getAnnotation()
		}
	}
}
