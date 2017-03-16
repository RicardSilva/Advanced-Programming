public class ClassCommand implements Command {
	private String name;
	
	public ClassCommand() throws InvalidArgumentsException {
		throw new InvalidArgumentsException("USAGE: Class <name>");
	}
	
	public ClassCommand(String name) {
		this.name = name;
	}

	public Object execute() {
		// try
		// {
			// Class workingClass = Class.forName(className);
			// return workingClass;
		// }
		// catch (ClassNotFoundException ex)
		// {
		// 	System.out.println("The given class doesn't exist!");
		// 	return null;
		// }
		return new Object();
	}
}
