import java.io.Console;
import java.lang.reflect.Constructor;
import java.util.Dictionary;
import java.util.Scanner;

public class Shell {
	private Scanner console;
	
	public Shell(Scanner c)
	{
		console = c;
	}
		
	public static void main(String[] args)
	{
		Scanner console = new Scanner(System.in);
		Shell s = new Shell(console);
		while (true)
		{
			Command command = s.getCommand();
			Object last = command.execute();
		}
		
	}
	
	public Command getCommand()
	{
		System.out.println("Input command:");
		String[] line = console.nextLine().split(" ");
		String cmd = line[0];
		String arg1 = line[1];
		if(line.length < 2)
			System.out.println("Not enough arguments!");
		
		String name =  cmd + "Command";
		try
		{
			Class commandClass = Class.forName(name);
			Constructor cons = commandClass.getConstructor(String.class);
			Command command = (Command) cons.newInstance(arg1);
			return command;
		}
		catch (ClassNotFoundException ex)
		{
			return new UnknownCommand();
		}
		catch (Exception ex)
		{
			System.out.println("Erro a executar o comando");
			return null;
		}
		
		
		
	
	}
}
