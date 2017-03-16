import java.lang.reflect.Constructor;
import java.util.Scanner;

public class Shell {
	private Scanner scanner = new Scanner(System.in);
	
	public static void main(String[] args) {
		Command command;
		Object result;
		
		Shell shell = new Shell();
		
		do {
			command = shell.getCommand();
			if (command != null) {
				result = command.execute();
			}
		} while (command == null || command.getClass() != ExitCommand.class);
	}
	
	public Command getCommand() {
		Command commandObj;
		
		System.out.println("Input command:");
		String[] input = scanner.nextLine().split(" ");
		if (input[0] == null) {
			return null;
		}
	
		//Create instance of the selected command
		try {
			Class commandClass = Class.forName(input[0] + "Command");
			
			if(input.length == 1) {
				commandObj = (Command) commandClass.newInstance();
			} else {
				Constructor cons = commandClass.getConstructor(String.class);
				commandObj = (Command) cons.newInstance(input[1]);
			}
			return commandObj;
		}
		catch (ClassNotFoundException ex) {
			return new UnknownCommand(); // HERE
		}
		catch (Exception ex) {
			System.out.println("ERROR: " + ex.getMessage());
			return null;
		}
	}
}
