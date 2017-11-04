package Uebung1;

import java.util.concurrent.Semaphore;

public class DiningPhilosophers{
	// Variante 0: Deadlock
	// Variante 1: einer der Philosophen nimmt sein Besteck in einer anderen Reihenfolge
	// Variante 2: die Philosophen schauen bevor sie sich das zweite nehmen, ob es erreichbar ist
	private static int variante = 0;
	private static int number = 5;
	private static long maxSleep = 500;
	
	private static void sleep(long time) {
		try {
			Thread.sleep(time);
		} catch (InterruptedException e) {
		}
	}
	
	public static void main(String[] args) {
		switch(variante) {
		case 0:
			variante0();
			break;
		case 1:
			variante1();
			break;
		case 2:
			variante2();
			break;
		}
	}
	
	private static void variante0() {
		if(number < 2){
			return;
		}
		
		Object[] sticks = new Object[number];
		NormalPhilosopher[] phils = new NormalPhilosopher[number];
		
		for(int i = 0; i < number; i++) {
			sticks[i] = new Object();
		}
		
		for(int i = 0; i < number; i++) {
			phils[i] = new NormalPhilosopher(i, sticks[i], sticks[(i+1) % number]);
			new Thread(phils[i]).start();
		}
	}
	
	private static void variante1() {
		if(number < 2){
			return;
		}
		
		Object[] sticks = new Object[number];
		NormalPhilosopher[] phils = new NormalPhilosopher[number];
		
		for(int i = 0; i < number; i++){
			sticks[i] = new Object();
		}
		
		phils[0] = new NormalPhilosopher(0, sticks[0], sticks[1]);
		new Thread(phils[0]).start();
		for(int i = 1; i < number; i++){
			phils[i] = new NormalPhilosopher(i, sticks[i], sticks[(i+1) % number]);
			new Thread(phils[i]).start();
		}
	}
	
	private static void variante2() {
		if(number < 2) {
			return;
		}
		
		Semaphore[] sticks = new Semaphore[number];
		WatchingPhilosopher[] phils = new WatchingPhilosopher[number];
		
		for(int i = 0; i < number; i++){
			sticks[i] = new Semaphore(1);
		}
		
		for(int i = 0; i < number; i++){
			phils[i] = new WatchingPhilosopher(i, sticks[i], sticks[(i+1) % number]);
			new Thread(phils[i]).start();
		}
	}
	
	private static class NormalPhilosopher implements Runnable {
		
		private Object first;
		private Object second;
		private int id;
		
		public NormalPhilosopher(int id, Object first, Object second) {
			this.id = id;
			this.first = first;
			this.second = second;
		}
		
		public void run() {
			while (true) {
				System.out.println("Philosopher " + id + " is thinking!");
				sleep((long) (maxSleep * Math.random()));
				
				synchronized (first) {
					synchronized (second) {
						System.out.println("Philosopher " + id + " is eating!");
						sleep((long) (maxSleep * Math.random()));
					}
				}
			}
		}
		
	}

	private static class WatchingPhilosopher implements Runnable {

		private Semaphore first;
		private Semaphore second;
		private int id;
		
		public WatchingPhilosopher(int id, Semaphore first, Semaphore second) {
			this.id = id;
			this.first = first;
			this.second = second;
		}
		
		@Override
		public void run() {
			try {
			while (true) {
				System.out.println("Philosopher " + id + " is thinking!");
				sleep((long) (maxSleep * Math.random()));
				
				first.acquire();
				while(second.availablePermits() == 0){
					first.release();
					Thread.yield();
					first.acquire();
				}
				second.acquire();
				System.out.println("Phiosopher " + id + " is eating!");
				sleep((long) (maxSleep * Math.random()));
				second.release();
				first.release();
				
			}
			} catch (InterruptedException e){}
			
		}
		
	}
	
}
