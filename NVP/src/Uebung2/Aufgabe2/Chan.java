package Uebung2.Aufgabe2;

public class Chan<T> {
  private MVar<MVar<ChanElem<T>>> read;
  private MVar<MVar<ChanElem<T>>> write;

  public Chan() {
    MVar<ChanElem<T>> hole = new MVar<ChanElem<T>>();
    read = new MVar<MVar<ChanElem<T>>>(hole);
    write = new MVar<MVar<ChanElem<T>>>(hole);
  }

  public T read() {
    MVar<ChanElem<T>> rEnd = read.take();
    ChanElem<T> elem = rEnd.take();
    read.put(elem.getNext());
    return elem.getValue();
  }

  public void write(T value) {
    MVar<ChanElem<T>> newHole = new MVar<ChanElem<T>>();
    MVar<ChanElem<T>> wEnd = write.take();
    wEnd.put(new ChanElem<T>(value,newHole));
    write.put(newHole);
  }

  public boolean isEmpty() {
    MVar<ChanElem<T>> rEnd = read.read();
    MVar<ChanElem<T>> wEnd = write.read();
    return rEnd==wEnd;
  }
}
