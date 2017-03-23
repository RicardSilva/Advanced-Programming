package ist.meic.pa;

import javassist.*;
import java.util.HashMap;
import java.util.Map;
import java.lang.reflect.*;

public class KeywordTranslator implements Translator{
	
	public void start(ClassPool pool) throws NotFoundException, CannotCompileException {
	}
	
	private CtField.Initializer getInitializer(CtField field, String defaultValue)
	{
        try
        {
        //Get the initializer by reflection
        Class initClass = CtField.Initializer.class;
        
        CtClass ctClass = field.getType();
        
        if(ctClass.isPrimitive())
        {
            if (ctClass == CtClass.intType) {
                int iVal = Integer.parseInt(defaultValue);
                return CtField.Initializer.constant(iVal);
            }
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
	
	
	private void modifyConstructor(CtClass ctClass, CtConstructor ctConst, KeywordArgs annotation) {
        Map<String,String> arguments = getArguments(annotation);
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

	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
		
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
