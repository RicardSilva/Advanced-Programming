package ist.meic.pa;

import javassist.*;
import java.lang.reflect.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class KeyConstructors {
	private Map<String, Method> setups = new HashMap<String, Method>();

	public KeyConstructors() {
		
	}

	public void buildConstructors() {
		// ClassPool pool = ClassPool.getDefault();
		// for(CtClass ctClass : pool.get("Widget"));
	}
	
	public void buildWidget() {
		try {
			ClassPool pool = ClassPool.getDefault();
			CtClass ctClass = pool.get("Widget");
			
			for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
				Object[] annotations = ctMethod.getAnnotations();
				if ((annotations.length == 1) && (annotations[0] instanceof KeywordArgs)) {
					System.out.println("ANNOTATION!");
				}
			}
		} catch (Exception ex) {
			System.out.println("ERROR: " + ex.getClass());
		}
	}


	public static void main(String[] args) throws Exception {
		// TO DO: read test from args
		
		Scanner console = new Scanner(System.in);
		String[] commands = console.nextLine().split(" ");
		
		//Set our own translator
		KeyConstructors keyC = new KeyConstructors();
		Translator translator = new KeywordTranslator();
		ClassPool pool = ClassPool.getDefault();
		Loader classLoader = new Loader();
		classLoader.addTranslator(pool, translator);
		
		try
		{
            classLoader.run(commands[0], new String[1]);
		}
		catch (Throwable e)
		{
            System.out.println("Throwable in test class!");
		}
		//Call the test class
		/*Class testClass = Class.forName(commands[0]);
		Method main = testClass.getMethod("main", String[].class);

		main.invoke(null, (Object[]) new String[1]);*/
		
		console.close();
	}
}
