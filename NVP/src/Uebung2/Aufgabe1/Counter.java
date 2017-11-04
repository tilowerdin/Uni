package Uebung2.Aufgabe1;

import java.util.HashSet;
import java.util.Set;

public class Counter implements Runnable {

	private boolean running;
	private Set<CounterGUI> frames;
	private int value;
	private long waitTime;
	private String name;
	private Thread executingThread;

	public Counter(long millis, String name) {
		waitTime = millis;
		this.name = name;
		running = true;
		value = 0;
	}
	
	public Counter(long millis, String name, int startValue, boolean running) {
		this(millis, name);
		this.value = startValue;
		this.running = running;
	}

	@Override
	public void run() {
		System.out.println("Frame erstellen");
		frames = new HashSet<>();
		frames.add(new CounterGUI(this, value));
		executingThread = Thread.currentThread();
		try {
			while (!executingThread.isInterrupted()) {
				synchronized (this) {
					sleep();
					while (!running) {
						this.wait();
					}
					value++;
				}
				notifyAllFrames();
				Thread.yield();
			}
		} catch (InterruptedException e) {
		}

	}

	private void sleep() throws InterruptedException {
		Thread.sleep(waitTime);
	}

	private void notifyAllFrames() {
		for (CounterGUI frame : frames) {
			frame.notify(value);
		}
	}

	public synchronized void start() {
		notify();
		running = true;
	}

	public synchronized void stop() {
		running = false;
	}

	public synchronized void close(CounterGUI frame) {
		frames.remove(frame);
		if (frames.size() == 0) {
			// interrupt running Thread
			executingThread.interrupt();
		}
	}
	
	public synchronized void copyCounter() {
		new Thread(new Counter(waitTime, name, value, running)).start();
	}
	
	public synchronized void cloneCounter() {
		frames.add(new CounterGUI(this, value));
	}

}
