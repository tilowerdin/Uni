package Uebung1;

import java.lang.Thread.State;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

public class Test {

	public static void main(String[] args) {
		
		// je 50 Producer und Consumer erstellen und konsumieren Integer
		// konsumierte Integer werden in ein Set abgelegt.
		// wenn ein Integer geschrieben werden soll, der bereits vorhanden ist
		// gibt es eine Ausgabe in der Konsole
		// jeder Producer und Consumer produziert bzw. konsumiert 20 mal
		// es kann dabei natürlich vorkommen, dass nicht alle Threads
		// fertig werden. Allerdings blockiert kein Thread
		
		MVar<Integer> mvar = new MVar<>();
		Set<Integer> redIntegers = new HashSet<>();
		Set<Thread> threadSet = new HashSet<>();
		
		for (int i = 0; i < 50; i++) {
			Producer p = new Producer(i, mvar, redIntegers);
			threadSet.add(p);
			p.start();
		}
		
		for (int i = 50; i < 100; i++) {
			Consumer c = new Consumer(i, mvar, redIntegers);
			threadSet.add(c);
			c.start();
		}
		
		int terminated = 0;
		int waiting = 0;
		while(terminated + waiting != 100) {
			terminated = 0;
			waiting = 0;
			try {
				Thread.sleep(2000);
			} catch (InterruptedException e) {}
			for(Thread t : threadSet) {
				if(t.getState() == State.TERMINATED) {
					terminated++;
				}
				if(t.getState() == State.WAITING) {
					waiting++;
				}
			}
		}
		
		System.out.println(terminated + " Threads are TERMINATED,\n" + waiting + " Threads are WAITING");
		for(Thread t : threadSet)
			t.interrupt();
	}
	
	private static class Producer extends Thread {
		
		private Random r;
		private MVar<Integer> mvar;
		private Set<Integer> redIntegers;
		private static int counter = 0;
		private int id;
		
		public Producer (int id, MVar<Integer> mvar, Set<Integer> redIntegers) {
			r = new Random();
			this.mvar = mvar;
			this.redIntegers = redIntegers;
			this.id = id;
		}
		
		@Override
		public void run() {
			try {
				for(int i = 0; i < 20; i++){
					switch(r.nextInt(5)) {
					case 0:
						mvar.put(getCounter());
						break;
					case 1:
						mvar.put(getCounter(), 100);
						break;
					case 2:
						mvar.tryPut(getCounter());
						break;
					case 3:
						mvar.overWrite(getCounter());
						break;
					case 4:
						int red = mvar.swap(getCounter());
						synchronized (redIntegers){
							if (!redIntegers.add(red)){
								System.out.println("got an already added int after swap " + red);
							}
						}
					}
				}
			} catch (InterruptedException e) {}
		}
		
		private static synchronized int getCounter() {
			int i = counter;
			counter++;
			return counter;
		}
		
	}

	private static class Consumer extends Thread {
		
		private MVar<Integer> mvar;
		private Set<Integer> redIntegers;
		private Random r;
		private int id;
		
		public Consumer(int id, MVar<Integer> mvar, Set<Integer> redIntegers){
			this.redIntegers = redIntegers;
			r = new Random();
			this.mvar = mvar;
			this.id = id;
		}
		
		@Override
		public void run() {
			try{
				for(int i = 0; i < 20; i++) {
					Integer o;
					switch (r.nextInt(4)){
					case 0:
						o = mvar.take();
						addToSet(o);
						break;
					case 1:
						o = mvar.take(100);
						addToSet(o);
						break;
					case 2:
						mvar.clear();
						break;
					case 3:
						o = mvar.tryTake();
						addToSet(o);
						break;
					}
				}
				
			} catch (InterruptedException e) {}
		}
		
		private void addToSet(Integer o) {
			if(o == null)
				return;
			synchronized (redIntegers) {
				if(!redIntegers.add(o)) {
					System.out.println("got an already added int");
				}
			}
		}
		
	}
	
}
