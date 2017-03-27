package ist.meic.pa;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;

public class KeywordTranslator implements Translator {
	
	/////////////////////////////
	///// Auxiliary Methods /////
	/////////////////////////////
	
	// Parse @KeywordArgs arguments and return them as a map
	private Map<String,String> parseArguments(KeywordArgs annotation) {
		Map<String,String> arguments = new HashMap<String,String>();
		String[] givenArgs = annotation.value().split(",");
		
		for(String argumentPair : givenArgs) {
            String[] pair = argumentPair.split("=");
            String argName = pair[0].trim();
			String defValue = null;
			if(pair.length == 2) {
				defValue = pair[1].trim();
			}
            arguments.put(argName, defValue);
		}
        return arguments;
	}

	private Set<String> getKeywords (KeywordArgs annotation) {
		Set<String> keywords =  new HashSet<String>();
		String[] givenArgs = annotation.value().split(",");
		for(String argumentPair : givenArgs)
		{
            String[] pair = argumentPair.split("=");
            String keword = pair[0].trim();
			keywords.add(keword);
		}
        return keywords;
		
	}

	//////////////////////////////////
	///// Code-Injection Methods /////
	//////////////////////////////////

	//Code to be injected in the constructor
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
							//There was an error assigning the value to the field
		code += "			throw new IllegalArgumentException(\"The argument was not defined in the constructor.\");";
		code += "	}";
		code += "}";
		
		ctConst.insertBeforeBody(code);
	}


	private void InjectFieldInitializers(CtConstructor ctConst, KeywordArgs annotation) throws CannotCompileException {
		String code = "";
		Map<String,String> arguments = parseArguments(annotation);
		
		for(String arg : arguments.keySet()){
			if(arguments.get(arg) != null)
				code += arg + "=" + arguments.get(arg) + ";";
            
        }
		ctConst.insertBeforeBody(code);
	}


	private void InjectContainsKeywordMethod(CtClass ctClass)
			throws NotFoundException, ClassNotFoundException, CannotCompileException {
		CtClass currentClass = ctClass;
		
		Set<String> keywordSet = new HashSet<String>();
		while(currentClass != null) {
			CtConstructor[] constructors = currentClass.getConstructors();
			for(CtConstructor constructor : constructors) {
				Object annotation = constructor.getAnnotation(KeywordArgs.class);
				if(annotation != null) {
					keywordSet.addAll(getKeywords((KeywordArgs) annotation));
				}
			}
			currentClass = currentClass.getSuperclass();
		}
		
		// Build condition string
		String list = "";
		for(String key : keywordSet){
			list += "keyword.equals(\"" + key + "\") || ";
		}
		list += "false";
		
		String setCode = "static boolean containsKeyword(String keyword) { return " + list + "; }";

		CtMethod method = CtMethod.make(setCode, ctClass);
		ctClass.addMethod(method);
	}

	///////////////////////////
	///// Default Methods /////
	///////////////////////////

	public void start(ClassPool pool) throws NotFoundException, CannotCompileException {
		// Do nothing
	}
	
	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
		
		// For each constructor of the loaded class
		CtClass ctClass = pool.getCtClass(className);
		CtClass objectArr = pool.getCtClass(Object[].class.getName());
		CtClass[] params = {objectArr};
		KeywordArgs annotation = null;
		CtConstructor ctConst = null;
		try {
			ctConst =  ctClass.getDeclaredConstructor(params);
			annotation = (KeywordArgs) ctConst.getAnnotation(KeywordArgs.class);
		}
		catch (NotFoundException ex)
		{
			//The constructor doesn't exist. Nothing else to do
			return;
		}
        catch (ClassNotFoundException ex) {
        	//The annotation doesn't exist. Nothing else to do.
        	return;
        }
		
		
		try {
			// If @KeywordArgs annotation is present modify constructor 
			if(annotation != null) {
				InjectContainsKeywordMethod(ctClass);	
				InjectUpdateFieldsCode(ctConst);
				InjectFieldInitializers(ctConst, annotation);
	        }
		} catch (ClassNotFoundException e) {
			//TODO ver de onde vieram estas exceções
		}
		
	}
	
	
	///////////////////////////
	///// Deprecated Code /////
	///////////////////////////
	
		// private CtField.Initializer getInitializer(CtField field, String defaultValue) {
 //       try {
 //       //Get the initializer by reflection
        
 //       CtClass ctClass = field.getType();
        
 //       if(ctClass.isPrimitive())
 //       {
 //           if (ctClass == CtClass.intType) {
 //               int iVal = Integer.parseInt(defaultValue);
 //               return CtField.Initializer.constant(iVal);
 //           }
 //           //TODO os outros tipos primitivos
 //           else
 //           {
 //               throw new UnsupportedOperationException();
 //           }
 //       }
 //       else
 //       {
 //           throw new UnsupportedOperationException();
 //       }
        
 //           /*System.out.println("CtClass: " + ctClass);
 //           Class typeClass = field.getType().toClass();
 //           System.out.println("Class: " + typeClass);
 //           Method method = initClass.getMethod("constant", typeClass);
            
 //           CtField.Initializer initializer = (CtField.Initializer) method.invoke(null, typeClass.cast(defaultValue));
 //           return initializer;*/
 //       }
 //       catch(Throwable ex)
 //       {
 //           //TODO change
 //           System.out.println("Erro no getInitializer" + ex);
 //           return null;
 //       }
	// }
	
	// //Build the default initializers
	// private void buildDefaultInitializers1(CtClass ctClass, Map<String,String> arguments) {
		
 //       for(String arg : arguments.keySet()){
 //           try {
            	
 //               CtField field = ctClass.getField(arg);
 //               ctClass.removeField(field);
 //               String defaultValue = arguments.get(arg);
 //               CtField.Initializer initializer = getInitializer(field, defaultValue);
 //               ctClass.addField(field, initializer);
 //               System.out.println(field);
 //           }
 //           catch (NotFoundException ex)
 //           {
 //               //We need to throw this exceptions because we can't save the argument (nor guess it's type).
 //               throw new RuntimeException("Undeclared field: " + arg);
 //           }
 //           catch (CannotCompileException ex)
 //           {
 //               throw new RuntimeException("Cannot compile field" + arg);
 //           }
            
 //       }
	// }
	
	// private void modifyConstructor(CtClass ctClass, CtConstructor ctConst, KeywordArgs annotation) {
		
	// 	Map<String,String> arguments = parseArguments(annotation);
		
	// 	//First we set the default initializers
	// 	//buildDefaultInitializers(ctClass, arguments);
		
	// 	//Insert keyword arguments method in the class
	// 	addKeywordSet(ctClass);
	// 	String defaultValues = buildDefaultInitializers(arguments);
	// 	//Now we inject the code to fill the arguments in the constructor
	// 	String code = codeArgumentsReflection();
	// 	try {
	// 		ctConst.insertBeforeBody(code);
	// 		ctConst.insertBeforeBody(defaultValues);
	// 	} catch (CannotCompileException e) {
	// 		// TODO Auto-generated catch block
	// 		e.printStackTrace();
	// 	}
	// }
}
