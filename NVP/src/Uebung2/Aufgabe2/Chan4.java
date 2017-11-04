package Uebung2.Aufgabe2;

import java.util.Collection;

public class Chan4<T> {
	private MVar<MVar<ChanElem<T>>> read;
	private MVar<MVar<ChanElem<T>>> write;
	private int maxCap;
	private int count;

	public Chan4(int capacity) {
		count = 0;
		maxCap = capacity;
		MVar<ChanElem<T>> hole = new MVar<ChanElem<T>>();
		read = new MVar<MVar<ChanElem<T>>>(hole);
		write = new MVar<MVar<ChanElem<T>>>(hole);
	}

	public T read() throws InterruptedException {
		MVar<ChanElem<T>> rEnd = read.take();
		ChanElem<T> elem = rEnd.take();
		read.put(elem.getNext());
		synchronized (this) {
			count--;
			notify();
		}
		return elem.getValue();
	}

	public void write(T value) throws InterruptedException {
		MVar<ChanElem<T>> newHole = new MVar<ChanElem<T>>();
		synchronized(this){
			while(count >= maxCap){
				wait();
			}
			count++;
		}
		MVar<ChanElem<T>> wEnd = write.take();
		wEnd.put(new ChanElem<T>(value, newHole));
		write.put(newHole);
	}

	/**
	 * testet ob der read pointer und der write pointer auf die selbe Stelle
	 * der Chan zeigen. wenn dem so ist, dann ist die Chan leer.
	 * @return
	 * @throws InterruptedException
	 */
	public boolean isEmpty() throws InterruptedException
	{
		MVar<ChanElem<T>> rEnd = read.take();
		MVar<ChanElem<T>> wEnd = write.take();
		boolean equals = rEnd == wEnd;
		read.put(rEnd);
		write.put(wEnd);
		return equals;
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
		synchronized(this){
			while(count >= maxCap){
				wait();
			}
			count++;
		}
		MVar<ChanElem<T>> mvar = new MVar<>(new ChanElem<>(elem, read.take()));
		read.put(mvar);
	}
}
