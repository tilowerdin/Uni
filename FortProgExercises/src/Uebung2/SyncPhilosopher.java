package Uebung2;

import java.util.concurrent.Semaphore;

public class SyncPhilosopher extends Thread {

	private int num;
	private Semaphore left, right;

	public SyncPhilosopher(int num, Semaphore left, Semaphore right) {
		this.num = num;
		this.left = left;
		this.right = right;
	}

	private void snooze() {
		try {
			Thread.sleep((long) (1000 * Math.random()));
		} catch (InterruptedException _) {
		}
	}

	public void run() {
		while (true) {
			System.out.println("Philosopher " + num + " is thinking");
			snooze();
			left.acquireUninterruptibly();
			while (!right.tryAcquire()) {
				left.release();
				left.acquireUninterruptibly();
			}
			System.out.println("Philosopher " + num + " is eating");
			snooze();
			right.release();

			left.release();
		}
	}

	public static void main(String[] args) {
		int count = 5;

		if (args.length > 0) {
			try {
				count = Integer.parseInt(args[0]);
			} catch (NumberFormatException _) {
			}
		}

		Semaphore[] sticks = new Semaphore[count];
		SyncPhilosopher[] phils = new SyncPhilosopher[count];

		for (int i = 0; i < count; ++i) {
			sticks[i] = new Semaphore(1);
		}

		for (int i = 0; i < count; ++i) {
			phils[i] = new SyncPhilosopher(i, sticks[i],
					sticks[(i + 1) % count]);
			phils[i].start();
		}
	}

}
