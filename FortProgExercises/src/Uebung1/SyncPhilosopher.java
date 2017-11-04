package Uebung1;

public class SyncPhilosopher implements Runnable {

	private int id = 0;
	private Object left;
	private Object right;

	public SyncPhilosopher(int id, Object left, Object right) {
		this.id = id;
		this.left = left;
		this.right = right;
	}

	private void sleep(long time) {
		try {
			Thread.sleep(time);
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
	}

	public void run() {
		while (true) {
			long sleepCount = (long) (10000 * Math.random());

			System.out.println("Philosopher " + id + " is thinking.");
			sleep(sleepCount);

			synchronized (left) {
				synchronized (right) {
					System.out.println("Philosopher " + id + " is eating.");
					sleep(sleepCount);
				}
			}
		}
	}

	public static void main(String[] args) {
		int count = 5;
		Object[] sticks = new Object[count];
		SyncPhilosopher[] phils = new SyncPhilosopher[count];

		sticks[0] = new Object();
		for (int i = 1; i < count; i++) {
			sticks[i] = new Object();

			phils[i] = new SyncPhilosopher(i, sticks[i - 1], sticks[i]);
			new Thread(phils[i]).start();
		}
		phils[0] = new SyncPhilosopher(0, sticks[0], sticks[count - 1]);
		new Thread(phils[0]).start();
	}

}
