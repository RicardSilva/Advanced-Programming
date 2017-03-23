public class SetCommand implements Command {
	private String name;

	public SetCommand() throws IllegalArgumentException {
		throw new IllegalArgumentException("USAGE: Set <name>");
	}
	
	public SetCommand(String name) {
		this.name = name;
	}

	public Object execute() {
		// TODO Auto-generated method stub
		return null;
	}
}