// Tests if:
// - params with no default values are initialized correctly (more types)

public class TestE {
	public static void main(String[] args) {
		System.err.println(new MixKeys());
		System.err.println(new MixKeys("l", 20000, "v", false));
	}
}
