public class ClassCommand implements Command {
	private String name;
	
	public ClassCommand() throws IllegalArgumentException {
		throw new IllegalArgumentException("USAGE: Class <name>");
	}
	
	public ClassCommand(String name) {
		this.name = name;
	}

	public Object execute() {
		try {
			Class targetClass = Class.forName(name);
			return targetClass;
		}
		catch (Exception ex) {
			System.out.println("ERROR: " + ex.getClass());
			return null;
		}
	}
}
