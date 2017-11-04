package Uebung2.Aufgabe2;

public class MVar<T> {

	T content;
	boolean empty;
	Object r = new Object();
	Object w = new Object();

	public MVar() {
		content = null;
		empty = true;
	}

	public MVar(T o) {
		content = o;
		empty = false;
	}

	public T take() throws InterruptedException {
		synchronized (r) {
			while (empty) { r.wait(); }
			// here empty is false 
			synchronized (w) {
				empty = true;
				w.notify();
				return content;
			}
		}
	}
	
	public T take(long timeout) throws InterruptedException {
		synchronized (r) {
			if (empty) { r.wait(timeout); }
			if (empty) { return null; } // here we could throw an Exception or return null
			synchronized (w) {
				empty = true;
				w.notify();
				return content;
			}
		}
	}

	public void put(T o) throws InterruptedException {
		synchronized (w) {
			while (!empty) { w.wait(); }
			// here empty is true
			synchronized (r) {
				empty = false;
				r.notify();
				content = o;
			}
		}
	}

	public void put(T o, long timeout) throws InterruptedException {
		synchronized (w) {
			if (!empty) { w.wait(timeout); }
			if (!empty) { return; } // here we could throw an Exception or just return, another option might be a boolean returntype
			// here empty is true
			synchronized (r) {
				empty = false;
				r.notify();
				content = o;
			}
		}
	}
	
	public T read()  throws InterruptedException {
		synchronized (r) {
			while (empty) { r.wait(); }
			// here empty is false 
			synchronized (w) {
				r.notify();
				return content;
			}
		}
	}

	public void clear() {
		synchronized (r) {
			if(empty) return;
			synchronized (w) {
				empty = true;
				w.notify();
			}
		}
	}
		
	public boolean tryPut(T v) {
		synchronized (w) {
			if (!empty) return false;
			synchronized (r){
				empty = false;
				r.notify();
				content = v;
				return true;
			}
		}
	}
	
	public T tryTake() {
		synchronized (r) {
			if (empty) { return null; }
			// here empty is false 
			synchronized (w) {
				empty = true;
				w.notify();
				return content;
			}
		}
	}

	public void overWrite(T v) {
		synchronized (w){
			if (empty) {
				synchronized (r) {
					content = v;
					empty = false;
					r.notify();
				}
			} else { // !empty
				content = v;
			}
		}
	}

	public T swap(T v) throws InterruptedException{
		synchronized (r) {
			while(empty){ r.wait(); } 
			synchronized (w) {
				T temp = content;
				content = v;
				r.notify();
				return temp;
			}
		}
	}
}
