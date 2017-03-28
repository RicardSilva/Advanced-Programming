/*
KeyConstructors: Receives a class as argument and runs it using our own custom class loader
*/

package ist.meic.pa;

import javassist.Loader;
import javassist.Translator;
import javassist.ClassPool;
import java.util.Scanner;

public class KeyConstructors {

	public KeyConstructors() {
		
	}

	public static void main(String[] args) throws Exception {
		String className;
		
       		// Prompt for test name if none is given as argument
		if (args.length > 0) {
			className = args[0];
		} else {
			System.out.println("Input test:");
			Scanner scanner = new Scanner(System.in);
			className = scanner.nextLine().split(" ")[0];
			scanner.close();
		}
		
		// Set our own translator
		Translator translator = new KeywordTranslator();
		ClassPool pool = ClassPool.getDefault();
		Loader classLoader = new Loader();
		classLoader.addTranslator(pool, translator);
		
		try {
            		classLoader.run(className, new String[1]);
		}
		catch (Throwable t) {
			t.printStackTrace();
		}
	}
}
