public class IndexCommand implements Command {
	private int index;

	public IndexCommand() throws IllegalArgumentException {
		throw new IllegalArgumentException("USAGE: Index <int>");
	}
	
	public IndexCommand(String index) throws IllegalArgumentException {
		try {
			this.index = Integer.parseInt(index);
		}
		catch (NumberFormatException ex) {
			throw new IllegalArgumentException("Index command must receive an integer");
		}
	}

	public Object execute() {
		// TODO Auto-generated method stub
		return null;
	}
}
