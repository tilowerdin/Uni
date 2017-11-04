package Uebung1;

public class Counter implements Runnable {

	private int count = 0;
	private long waitMillis;
	private String name;
	
	public Counter(long millis, String name) {
		waitMillis = millis;
		this.name = name;
	}
	
	@Override
	public void run() {
		while(true) {
			sleep();
			count++;
			System.out.println(name + " " + count);
		}
	}
	
	private void sleep() {
		try {
			Thread.sleep(waitMillis);
		} catch (InterruptedException e) {}
	}
	
	public static void main(String[] args) {
		for(int i = 0; i < args.length; i++){
			try {
				new Thread(new Counter(Integer.parseInt(args[i]), "Counter " + i)).start();
			} catch (Exception e){}
		}
	}

}
