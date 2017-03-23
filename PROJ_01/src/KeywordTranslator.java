package ist.meic.pa;

import javassist.*;

public class KeywordTranslator implements Translator{
	
	public void start(ClassPool pool) throws NotFoundException, CannotCompileException {
	
	}
	
	private bool isKeyword(CtConstructor constructor)
	{
	}
	
	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
		System.out.println("CLASS LOADED:" + className);
		//Find the keword constructor
		CtClass loadedClass = pool.getCtClass(className);
		CtConstructor[] constructors = loadedClass.getConstructors();
		for(CtConstructor constructor : constructors){
            if(isKeyword(constructor)
            {
                System.out.println("Class " + className + " has constructor: " + constructor.toString());
            }
            
		}
	}
}
