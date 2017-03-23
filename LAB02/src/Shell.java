import java.lang.reflect.Method;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Map;
import java.util.HashMap;
import java.util.Scanner;

public class Shell {
	private Scanner scanner = new Scanner(System.in);
	
	public static void main(String[] args) {
		//Map<String, Object> variables = new HashMap<String, Object>();
		Object lastResult = null;
		Object newResult = null;
		
		Shell shell = new Shell();
		
		do {
			if (newResult != null) {
				printObject(newResult);
				lastResult = newResult;
			}
			newResult = shell.runCommand(lastResult);
		} while (newResult == null || newResult.getClass() != ExitCommand.class);
	}
	
	public static void printObject(Object obj) {
		if (obj instanceof Object[]) {
			int i = 0;
			for (Object o : (Object []) obj) {
				System.out.println("[" + i++ + "] " + o);
			}
		} else {
			System.out.println(obj);
		}
	}
	
	public Object runCommand(Object lastResult) {
		Command commandObj;
		Constructor cons;
		
		System.out.println("Input command:");
		String[] input = scanner.nextLine().split(" ");
		if (input[0].isEmpty()) {
			printObject(lastResult); // print current object
			return null;
		}
		
		
		//String[] args = input[1].split(" ");
	
		// Create instance of the selected command
		try {
			Class commandClass = Class.forName(input[0] + "Command");
			
			if(input.length == 1) {
				cons = commandClass.getConstructor();
				commandObj = (Command) cons.newInstance();
			} else {
				cons = commandClass.getConstructor(String.class);
				commandObj = (Command) cons.newInstance(input[1]);
			}
			return commandObj.execute();
		}
		catch (ClassNotFoundException ex) {
			// Try to call selected method (without args)
			try {
				System.out.println("Trying generic command: " + lastResult.getClass().getName() + "." + input[0]);
				Method method = lastResult.getClass().getMethod(input[0]);
				return method.invoke(lastResult);
			}
			catch (Exception ex2) { System.out.println("ERROR: " + ex2.getClass()); }
			return null;
		}
		catch (InvocationTargetException ex) { System.out.println(ex.getCause().getMessage()); }
		catch (Exception ex) { System.out.println("ERROR: " + ex.getClass()); }
		return null;
	}
}
