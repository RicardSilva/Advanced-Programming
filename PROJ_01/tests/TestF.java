// Tests if:
// - argument can be passed as long as it's present somewhere on the chain

public class TestF {
	public static void main(String[] args) {
		System.err.println(new KeyPlaces());
		System.err.println(new KeyVisited());
		System.err.println(new KeyVisited("visited", 2, "second", "Yharnam"));
		System.err.println(new KeyVisited("third", "Pthumerian"));
	}
}
