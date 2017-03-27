// Tests if:
// - arguments are optional;
// - fields are initialized to their default values;
// - fields are overritten if their value is specified;

public class TestA {
	public static void main(String[] args) {
		System.err.println(new Widget());
		System.err.println(new Widget("width", 80));
		System.err.println(new Widget("height", 30));
		System.err.println(new Widget("margin", 2));
		System.err.println(new Widget("width", 8, "height", 13, "margin", 21));
	}
}
