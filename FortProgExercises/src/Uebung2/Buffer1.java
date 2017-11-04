package Uebung2;

import java.util.concurrent.TimeoutException;
import java.lang.System;

/**
 * Single element buffer with synchronization.
 * 
 * @author FortProg team
 * 
 * @param <E>
 *            Type of the element
 */
public class Buffer1<E> {

	// element + empty flag
	private E content;
	private boolean empty;

	// synchronization objects
	private Object r = new Object();
	private Object w = new Object();

	public Buffer1() {
		empty = true;
	}

	public Buffer1(E content) {
		this.content = content;
		empty = false;
	}

	/**
	 * take the element from the buffer; suspends on an empty buffer.
	 * 
	 * @return element of the buffer
	 * @throws InterruptedException
	 */
	public E take() throws InterruptedException {
		synchronized (r) {
			while (empty) {
				r.wait();
			}
			synchronized (w) {
				empty = true;
				w.notify();
				return content;
			}
		}
	}

	/**
	 * put an element into the buffer; suspends on a full buffer
	 * 
	 * @param o
	 *            Object to put into
	 * @throws InterruptedException
	 */
	public void put(E o) throws InterruptedException {
		synchronized (w) {
			while (!empty) {
				w.wait();
			}
			synchronized (r) {
				content = o;
				empty = false;
				r.notify();
			}
		}
	}

	/**
	 * Return whether the buffer is empty
	 * 
	 * @return true if empty
	 */
	public boolean isEmpty() {
		return empty;
	}

	/**
	 * Read the element from the buffer without emptying it; suspends on an
	 * empty buffer.
	 * 
	 * @return element of the buffer
	 * @throws InterruptedException
	 */
	public E read() throws InterruptedException {
		synchronized (r) {
			while (empty) {
				r.wait();
			}
			synchronized (w) {
				r.notify();
				return content;
			}
		}
	}

	/**
	 * Try to put an element into the buffer; succeeds only for an empty buffer
	 * 
	 * @param elem
	 *            Element to put into
	 * @return true if successful
	 */
	public boolean tryPut(E elem) {
		synchronized (w) {
			if (!empty) {
				return false;
			}
			try {
				put(elem);
			} catch (InterruptedException e) {
			}
		}
		return true;
	}

	/**
	 * Overwrite the element in the buffer, even if if the buffer is empty
	 * 
	 * @param elem
	 *            Element to overwrite with
	 */
	public void overwrite(E elem) {
		synchronized (w) {
			if (empty) {
				synchronized (r) {
					content = elem;
					empty = false;
					r.notify();
				}
			} else {
				content = elem;
			}
		}
	}

	/**
	 * take with timeout. The timeout mechanism has to be handcrafted as there
	 * is no way to detect whether a wait() was left because of a timeout or a
	 * notify().
	 * 
	 * @param timeout
	 *            Maximum time to wait in milliseconds
	 * @return
	 * @throws InterruptedException
	 * @throws TimeoutException
	 *             if a timeout occurred
	 */
	public E take(long timeout) throws InterruptedException, TimeoutException {
		long time = System.currentTimeMillis();
		synchronized (r) {
			while (empty) {
				if (System.currentTimeMillis() - time > timeout) {
					throw new TimeoutException();
				}
				r.wait(timeout - (System.currentTimeMillis() - time));
			}

			synchronized (w) {
				empty = true;
				w.notify();
				return content;
			}
		}
	}
}
