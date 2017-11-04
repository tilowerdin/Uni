package Uebung2.Aufgabe2;

import java.util.Collection;

public class Chan1<T> {
	private MVar<MVar<ChanElem<T>>> read;
	private MVar<MVar<ChanElem<T>>> write;

	public Chan1() {
		MVar<ChanElem<T>> hole = new MVar<ChanElem<T>>();
		read = new MVar<MVar<ChanElem<T>>>(hole);
		write = new MVar<MVar<ChanElem<T>>>(hole);
	}

	public T read() throws InterruptedException {
		MVar<ChanElem<T>> rEnd = read.take();
		ChanElem<T> elem = rEnd.take();
		read.put(elem.getNext());
		return elem.getValue();
	}

	public void write(T value) throws InterruptedException {
		MVar<ChanElem<T>> newHole = new MVar<ChanElem<T>>();
		MVar<ChanElem<T>> wEnd = write.take();
		wEnd.put(new ChanElem<T>(value, newHole));
		write.put(newHole);
	}

	public boolean isEmpty() throws InterruptedException {
		MVar<ChanElem<T>> rEnd = read.read();
		MVar<ChanElem<T>> wEnd = write.read();
		return rEnd == wEnd;
	}

	/**
	 * f�gt alle Elemente der �bergebenen Collection zur Chan hinzu. Dabei ist
	 * nicht garantiert, dass diese Elemente alle hintereinander in der Chan
	 * landen. Es kann passieren, dass ein anderer Thread durch ein write oder
	 * ebenfalls addMultiple zwischenzeitlich Elemente hinzuf�gt.
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
	 * f�gt ein Element an das falsche Ende der Chan hinzu. Sofern kein weiteres
	 * unGet ausgef�hrt wird, ist das �bergebene Element das n�chste, das durch
	 * read gelesen wird.
	 * 
	 * @param elem
	 * @throws InterruptedException
	 */
	public void unGet(T elem) throws InterruptedException {
		MVar<ChanElem<T>> mvar = new MVar<>(new ChanElem<>(elem, read.take()));
		read.put(mvar);
	}
}
