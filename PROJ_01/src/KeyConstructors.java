package ist.meic.pa;

import javaassist.*;
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




	public static void main(String[] args) throws Exception {
		// TO DO: read test from args
		
		Scanner console = new Scanner(System.in);
		String[] commands = console.nextLine().split(" ");
		
		KeyConstructors keyC = new KeyConstructors();
		keyC.buildConstructors();
		
		Class testClass = Class.forName(commands[0]);
		Method main = testClass.getMethod("main", String[].class);

		main.invoke(null, new String[1]);
		
		console.close();
	}
}