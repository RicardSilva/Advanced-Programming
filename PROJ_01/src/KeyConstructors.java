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

	public static void main(String[] args) throws Exception {
		String className;
        // Prompt for test name if none is given as argument
		if (args.length > 0) {
			className = args[0];
		} else {
			Scanner scanner = new Scanner(System.in);
			className = scanner.nextLine().split(" ")[0];
			scanner.close();
		}		
		
		//Set our own translator
		KeyConstructors keyC = new KeyConstructors();
		Translator translator = new KeywordTranslator();
		ClassPool pool = ClassPool.getDefault();
		Loader classLoader = new Loader();
		classLoader.addTranslator(pool, translator);
		

		try
		{
            classLoader.run(className, new String[1]);
		}
		catch (Throwable e)
		{
            System.out.println("Throwable in test class!");
		}
		
	}
}
