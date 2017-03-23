package ist.meic.pa;

import javassist.*;
import javassist.ClassPool;
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
			
			for (CtConstructor ctConst : ctClass.getDeclaredConstructors()) {
				Object[] annotations = ctConst.getAnnotations();
				if ((annotations.length == 1) && (annotations[0] instanceof KeywordArgs)) {
					modifyConstructor(ctClass, ctConst, annotations);
				}
			}
		} catch (Exception ex) {
			System.out.println("ERROR: " + ex.getClass());
		}
	}
	
	public void modifyConstructor(CtClass ctClass, CtConstructor ctConst, Object[] annotations){
		System.out.println("TO DO");
	}

	public static void main(String[] args) throws Exception {
		String className;
		
		KeyConstructors keyC = new KeyConstructors();
		keyC.buildWidget();
		
		// Prompt for test name if none is given as argument
		if (args.length > 0) {
			className = args[0];
		} else {
			Scanner scanner = new Scanner(System.in);
			className = scanner.nextLine().split(" ")[0];
			scanner.close();
		}

		Class testClass = Class.forName(className);
		Method main = testClass.getMethod("main", String[].class);

		main.invoke(null, (Object[]) new String[1]);
	}
}