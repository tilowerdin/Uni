import java.util.*;

public class STM {
    public class RollbackException extends Exception {}
    public class RetryException extends Exception {}

    private static class Pair<A,B> {
        A first;
        B second;
        Pair(A first, B second) {this.first = first; this.second = second;}}

    public static class TVar<A> implements Comparable {
        private A value = null;
        private int version = 0;
        private List<Thread> susps = new LinkedList<>();
        private boolean locked = false;

        public TVar(A value) {this.value = value;}
        Pair<A,Integer> read() { return new Pair<>(value, version); }
        synchronized void core_write(A value) throws InterruptedException {
            this.value = value;
            version++;
            for(Thread t : susps) {
                t.interrupt(); }
            susps.clear();}
        synchronized void addSusp() {
            susps.add(Thread.currentThread()); }
        synchronized void removeSusp() {
            susps.remove(Thread.currentThread()); }
        synchronized void lock() {
            locked = true; }
        synchronized void unlock() {
            locked = false;
            notifyAll(); }

        @Override
        public int compareTo(Object o) { return Integer.compare(hashCode(), o.hashCode()); }
    }

    private Map<TVar, Integer> readSet = new TreeMap<>();
    private Map<TVar, Object> writeSet = new TreeMap<>();

    public interface Fun<A> { A call() throws RollbackException, RetryException; }

    public <A> A atomically(Fun<A> fun) throws InterruptedException {
        readSet = new TreeMap<>();
        writeSet = new TreeMap<>();
        A result = null;
        try {
            result = fun.call();
            TreeSet<TVar> allTVars = new TreeSet<>(readSet.keySet());
            allTVars.addAll(writeSet.keySet());
            for (TVar tvar : allTVars) {tvar.lock();}
            if (validate()) {
                commit();
                for (TVar tvar : allTVars) {tvar.unlock();}
            } else {
                for (TVar tvar : allTVars) {tvar.unlock();}
                result = atomically(fun); }
        } catch (RollbackException rollback) {
            result = atomically(fun);
        } catch (RetryException retry) {
            for (TVar tVar : readSet.keySet()) { tVar.lock(); }
            if (validate()) {
                susp(readSet.keySet());
            } else for (TVar tVar : readSet.keySet()) { tVar.unlock(); }
            result = atomically(fun); }
        return result;}

    public <A> A readTVar(TVar<A> tVar) throws ClassCastException,RollbackException {
        A value = (A) writeSet.get(tVar);
        if (value == null) {
            Pair<A, Integer> tVarValue = tVar.read();
            Integer readSetVersion = readSet.get(tVar);
            if (readSetVersion == null)
                readSet.put(tVar,tVarValue.second);
            else if (!readSetVersion.equals(tVarValue.second))
                throw new RollbackException();
            return tVarValue.first;
        } else return value; }

    public <A> void writeTVar(TVar<A> tvar, A value) { writeSet.put(tvar, value); }

    public void retry() throws RetryException { throw new RetryException(); }

    private boolean validate() {
        boolean valid = true;
        for (TVar tvar : readSet.keySet()) {
            valid = valid && (tvar.read().second.equals(readSet.get(tvar))); }
        return valid; }

    private void susp(Set<TVar> set) {
        for (TVar tvar : set) {tvar.addSusp(); tvar.unlock();}
        boolean suspend = true;
        while (suspend) {
            try{
                synchronized (this) {
                    wait();
                }
            } catch (InterruptedException e) {
                suspend = false;
            }
        }
        for (TVar tvar : set) {tvar.removeSusp();} }

    public void commit() throws InterruptedException { for (TVar tvar : writeSet.keySet()) {tvar.core_write(writeSet.get(tvar));} }
}
