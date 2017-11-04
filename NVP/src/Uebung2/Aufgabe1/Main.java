package Uebung2.Aufgabe1;

public class Main {

	/**
	 * @param args
	 */
	public static void main(String[] args) {
		for(String s : args) {
			try {
				new Thread(new Counter(Integer.parseInt(s), "counter " + s)).start();
			} catch (Exception e) {}
		}
	}

}
