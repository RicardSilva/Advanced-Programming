import java.lang.reflect.*;
import java.util.HashMap;
import java.util.Map;
import java.util.Scanner;

public class RunTests {
	private Map<String, Method> setups = new HashMap<String, Method>();
	private String[] commands;
	
	public RunTests(String[] commands) {
		this.commands = commands;
	}

	public static void main(String[] args) throws Exception {
		 Scanner console = new Scanner(System.in);
		 String commandString = console.nextLine();
		 
		 RunTests runTests = new RunTests(commandString.split(" "));

		 runTests.loadAllSetups();
		 runTests.runAllTests();
		 
	      console.close();
	   }

	private void loadAllSetups() throws ClassNotFoundException {
		for (Method m : Class.forName(commands[0]).getDeclaredMethods()) {
			if (m.isAnnotationPresent(Setup.class))	{
				try {
					Setup ann = m.getAnnotation(Setup.class);
					setups.put(ann.value(), m);
				}
				catch (NullPointerException ex)
				{
					System.out.println("Something went very wrong!");
				}
			}
		}
		
	}

	private void runAllTests() throws ClassNotFoundException {
		int passed = 0, failed = 0;
		 
	      for (Method m : Class.forName(commands[0]).getDeclaredMethods()) {
	         if (m.isAnnotationPresent(Test.class)) {
	            try {
	            	Test ann = m.getAnnotation(Test.class);
	            	String[] setupsList = ann.value();
	            	if(setupsList.length == 0 || setupsList[0].equals("*"))
	            	{
	            		for(Method setupMethod : setups.values())
	            		{
	            			setupMethod.setAccessible(true);
	            			setupMethod.invoke(null);
	            		}
	            	}
	            	else
	            	{
	            		for(String setupName : setupsList)
	            		{
	            			Method setupMethod = setups.get(setupName);
	            			setupMethod.setAccessible(true);
	            			setupMethod.invoke(null);
	            		}
	            	}
	            	m.setAccessible(true);
	            	m.invoke(null);
	               passed++;
	            } catch (Throwable ex) {
	               System.out.printf("Test %s failed: %s %n", m, ex.getCause());
	               failed++;
	            }
	         }
	      }
	      System.out.printf("Passed: %d, Failed %d%n", passed, failed);
	}
	 

}
