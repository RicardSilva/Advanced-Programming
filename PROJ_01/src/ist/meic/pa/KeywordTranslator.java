package ist.meic.pa;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;

public class KeywordTranslator implements Translator{
	
	public void start(ClassPool pool) throws NotFoundException, CannotCompileException {
	}
	
	private CtField.Initializer getInitializer(CtField field, String defaultValue)
	{
        try
        {
        //Get the initializer by reflection
        
        CtClass ctClass = field.getType();
        
        if(ctClass.isPrimitive())
        {
            if (ctClass == CtClass.intType) {
                int iVal = Integer.parseInt(defaultValue);
                return CtField.Initializer.constant(iVal);
            }
            //TODO os outros tipos primitivos
            else
            {
                throw new UnsupportedOperationException();
            }
        }
        else
        {
            throw new UnsupportedOperationException();
        }
        
            /*System.out.println("CtClass: " + ctClass);
            Class typeClass = field.getType().toClass();
            System.out.println("Class: " + typeClass);
            Method method = initClass.getMethod("constant", typeClass);
            
            CtField.Initializer initializer = (CtField.Initializer) method.invoke(null, typeClass.cast(defaultValue));
            return  initializer;*/
        }
        catch(Throwable ex)
        {
            //TODO change
            System.out.println("Erro no getInitializer" + ex);
            return null;
        }
	}
	
	static boolean containsKeyword(String keyword)
	{
		return keyword.equals("a") || keyword.equals("b") || false;
	}
	
	static java.util.Set<String> FOO = new HashSet<String>(Arrays.asList("a", "b"));
	private void modifyConstructor(CtClass ctClass, CtConstructor ctConst, KeywordArgs annotation) 
			throws IllegalArgumentException {
		Map<String,String> arguments = getArguments(annotation);
		//First we set the default initializers 
		buildDefaultInitializers(ctClass, arguments);
		
		//Insert keyword arguments method in the class
		addKeywordSet(ctClass,arguments);
		
		//Now we inject the code to fill the arguments in the constructor
		String code = codeArgumentsReflection(arguments);
		try {
			ctConst.insertBeforeBody(code);
		} catch (CannotCompileException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		
	}

	

	
	private void addKeywordSet(CtClass ctClass,Map<String,String> arguments ) {
		
		
		String list = "";
		if(arguments.size() > 0)
		{
			for(String key : arguments.keySet()){
				list += "keyword.equals(\"" + key + "\") || ";
			}
			//Add a trailing false to be able to compile
			list += "false";
			System.out.println(list);
		}
		
		
		
		
		String setCode = "static boolean containsKeyword(String keyword) {";
		setCode += "return " + list + ";}";
		try {
			CtMethod method = CtMethod.make(setCode, ctClass);
			ctClass.addMethod(method);
		} catch (CannotCompileException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		
	}

	//Code to be injected in the constructor
	private String codeArgumentsReflection(Map<String,String> arguments) {
		String code = "";
		code += "for (int i = 0; i < $1.length / 2 ; i++)	{";
		code += "	String fieldName = (String) $1[2*i];";
		code += "	if(!containsKeyword(fieldName))";
		code += "		throw new RuntimeException(\"Unrecognized keyword: \" + fieldName);";	
		code += "Object value = $1[2*i+1];";
		code += "	try {";
		code += "		java.lang.reflect.Field field = this.getClass().getDeclaredField(fieldName);";
		code += "		field.setAccessible(true);";
		code += "		field.set(this, value);";
		code += "	} catch (Throwable e) {";
						//There was an error assigning the value to the field
		code += "		throw new IllegalArgumentException(e);";
		code += "}}";	
		return code;
	}

	//Build the default initializers
	private void buildDefaultInitializers(CtClass ctClass, Map<String,String> arguments) {
		
        for(String arg : arguments.keySet()){
            try {
            	
                CtField field = ctClass.getField(arg);
                ctClass.removeField(field);
                String defaultValue = arguments.get(arg);
                CtField.Initializer initializer = getInitializer(field, defaultValue);
                ctClass.addField(field, initializer);
                System.out.println(field);
            }
            catch (NotFoundException ex)
            {
                //We need to throw this exceptions because we can't save the argument (nor guess it's type).
                throw new RuntimeException("Undeclared field: " + arg);
            }
            catch (CannotCompileException ex)
            {
                throw new RuntimeException("Cannot compile field" + arg);
            }
            
        }
	}

	private Map<String,String> getArguments(KeywordArgs annotation){
        String[] givenArgs = annotation.value().split(",");
		Map<String,String> arguments = new HashMap<String,String>();
		for(String argumentPair : givenArgs)
		{
            String[] pair = argumentPair.split("=");
            String argName = pair[0].trim();
            String defValue = pair[1].trim();
            arguments.put(argName, defValue);
		}
        return arguments;
	}

	public void onLoad(ClassPool pool, String className) throws NotFoundException {
		
		System.out.println("CLASS LOADED:" + className);
		//Find the keword constructor
		CtClass loadedClass = pool.getCtClass(className);
		CtConstructor[] constructors = loadedClass.getConstructors();
		for(CtConstructor constructor : constructors){
            try {
                Object annotation = constructor.getAnnotation(KeywordArgs.class);
                if(annotation != null) {
                    System.out.println("Class " + className + " has constructor: " + constructor.toString());
                    modifyConstructor(loadedClass, constructor,(KeywordArgs) annotation);
                }               
            }
            catch (ClassNotFoundException e){
                throw new NotFoundException(className);
            }
            
		}
	}
}
