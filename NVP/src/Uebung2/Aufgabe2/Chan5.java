package Uebung2.Aufgabe2;

import java.util.Collection;

public class Chan5<T> {
	private int maxCap;
	private int count;
	private int w;
	private int r;
	private T[] buffer;
	private Object read = new Object();
	private Object write = new Object();

	@SuppressWarnings("unchecked")
	public Chan5(int capacity) {
		count = 0;
		maxCap = capacity;
		buffer = (T[]) new Object[capacity];
		w = 0;
		r = 0;
	}

	private synchronized void incCount() {
		count++;
	}

	private synchronized void decCount() {
		count--;
	}

	public T read() throws InterruptedException {
		synchronized (read) {
			while (count <= 0) {
				read.wait();
			}
			T obj = buffer[r];
			r = (r + 1) % maxCap;
			if (count >= maxCap) {
				synchronized (write) {
					decCount();
					write.notifyAll();
				}
			} else {
				decCount();
			}
			return obj;
		}
	}

	public void write(T value) throws InterruptedException {
		synchronized (write) {
			while (count >= maxCap) {
				write.wait();
			}

			buffer[w] = value;
			w = (w + 1) % maxCap;

			if (count <= 0) {
				synchronized (read) {
					incCount();
					read.notifyAll();
				}
			} else {
				incCount();
			}
		}
	}

	/**
	 * prüft ob die Anzahl der Elemente in der Chan 0 ist.
	 * 
	 * @return
	 * @throws InterruptedException
	 */
	public synchronized boolean isEmpty() {
		return count == 0;
	}

	/**
	 * fügt alle Elemente der übergebenen Collection zur Chan hinzu. Dabei ist
	 * nicht garantiert, dass diese Elemente alle hintereinander in der Chan
	 * landen. Es kann passieren, dass ein anderer Thread durch ein write oder
	 * ebenfalls addMultiple zwischenzeitlich Elemente hinzufügt.
	 * 
	 * @param collection
	 * @throws InterruptedException
	 */
	public void addMultiple(Collection<T> collection) throws InterruptedException {
		for (T elem : collection) {
			write(elem);
		}
	}

	/**
	 * fügt ein Element an das falsche Ende der Chan hinzu. Sofern kein weiteres
	 * unGet ausgeführt wird, ist das übergebene Element das nächste, das durch
	 * read gelesen wird.
	 * 
	 * @param elem
	 * @throws InterruptedException
	 */
	public void unGet(T elem) throws InterruptedException {
		synchronized (write) {
			while (count >= maxCap) {
				write.wait();
			}
			synchronized (read) {
				r = (r - 1) % maxCap;
				buffer[r] = elem;
				incCount();
				read.notifyAll();
			}
		}
	}
}
