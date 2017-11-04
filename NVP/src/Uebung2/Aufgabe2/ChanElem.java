package Uebung2.Aufgabe2;

public class ChanElem<T> {

	private T value;
	private MVar<ChanElem<T>> next;

	public ChanElem(T value, MVar<ChanElem<T>> next) {
		this.value = value;
		this.next = next;
	}

	public T getValue() {
		return value;
	}

	public MVar<ChanElem<T>> getNext() {
		return next;
	}
}