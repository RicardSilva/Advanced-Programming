public class GetCommand implements Command {
	private String name;

	public GetCommand() throws IllegalArgumentException {
		throw new IllegalArgumentException("USAGE: Get <name>");
	}
	
	public GetCommand(String name) {
		this.name = name;
	}

	public Object execute() {
		// TODO Auto-generated method stub
		return null;
	}
}
