
public class ClassCommand extends Command {

	private String className;
	
	public ClassCommand(String arg1) {
		className = arg1;
	}

	@Override
	public Object execute() {
		try
		{
			Class workingClass = Class.forName(className);
			return workingClass;
		}
		catch (ClassNotFoundException ex)
		{
			System.out.println("The given class doesn't exist!");
			return null;
		}
		
	}
	
	

}
