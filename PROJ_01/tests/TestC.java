// Test if:
// - An exception is thrown if an argument that does not exist is given

public class TestC {
	public static void main(String[] args) {
		System.err.println(new ExtendedWidget("foo", 1, "bar", 2));
	}
}
