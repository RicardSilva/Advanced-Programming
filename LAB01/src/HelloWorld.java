package World;
import java.lang.reflect.*;
import java.io.Console;

public class HelloWorld implements Message{
	public void say() {
		System.out.println("Hello World!");
	}

	public static void main(String[] args) {
		Console console = System.console();
		String name = console.readLine("Enter input:");
		try 
		{
			
			Class world = Class.forName(name, false, null);
			Message obj = (Message) world.newInstance();
			obj.say();
		}
		catch (Throwable e)
		{
			System.out.println("Erro");
		}
		
		return;
	}
}
