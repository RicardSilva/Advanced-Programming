import java.lang.reflect.*;
import java.io.Console;

public class Main{
	public static void main(String[] args) {
		Console console = System.console();
		String name = console.readLine("Enter input:");
		try {
			Class world = Class.forName(name);
			Message obj = (Message) world.newInstance();
			obj.say();
		}
		catch (Exception e) {
			System.out.println("Erro");
		}
		
		return;
	}
}
