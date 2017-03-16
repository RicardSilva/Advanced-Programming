public class ExitCommand implements Command {
	
	public ExitCommand() {
		//pass
	}
	
	public ExitCommand(String arg) {
		//pass
	}

	public Object execute() {
		System.out.println("Exiting...");
		return null;
	}
}