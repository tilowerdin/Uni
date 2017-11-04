package Uebung1;

public class IntVar {

	private int value;
	
    /* Little helper to sleep a period of time */
    private static void sleep(long time) {
        try {
            Thread.sleep(time);
        } catch (InterruptedException e) {
        }
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = value;
    }

    public void doubleIt() {
        int n = getValue();
        n = n * 2;
        sleep(5);
        setValue(n);
    }

    public void incIt() {
        int n = getValue();
        n++;
        sleep(2);
        setValue(n);
    }

    public void syncDoubleIt() {
        synchronized (this) {
			doubleIt();
		}
    }

    public void syncIncIt() {
        synchronized (this) {
			incIt();
		}
    }

    /**
     * Execute two @link{Runnable}s concurrently, whereas the second one is
     * started after a given delay
     *
     * @param v
     *            IntVar both Runnables operate on
     *
     * @param pause
     *            Interval to sleep before starting the second Runnable
     *
     * @param r1
     *            first Runnable to be executed
     * @param r2
     *            second Runnable to be executed
     */
    private static void test(final IntVar v, long pause, Runnable r1,
            Runnable r2) {

        Thread t1 = new Thread(r1);
        Thread t2 = new Thread(r2);

        t1.start();
        sleep(pause);
        t2.start();

        // wait for both to finish
        try {
            t1.join();
            t2.join();
        } catch (InterruptedException e) {
        }

        // prints the value
        System.out.println(v.getValue());
    }

    // Yields 0
    private static void zero() {

        final IntVar v = new IntVar();

        // You might want to adjust this value
        long sleepingTime = 1;

        Runnable r1 = new Runnable() {
            public void run() {
                v.incIt();
            }
        };

        Runnable r2 = new Runnable() {
            public void run() {
                v.doubleIt();
            }
        };

        test(v, sleepingTime, r1, r2);
    }

    // Yields 1
    private static void one() {

        final IntVar v = new IntVar();

        // You might want to adjust this value
        long sleepingTime = 10;

        Runnable r1 = new Runnable() {
            public void run() {
                v.doubleIt();
            }
        };

        Runnable r2 = new Runnable() {
            public void run() {
                v.incIt();
            }
        };

        test(v, sleepingTime, r1, r2);
    }

    // Yields 2
    private static void two() {

        final IntVar v = new IntVar();

        // You might want to adjust this value
        long sleepingTime = 10;

        Runnable r1 = new Runnable() {
            public void run() {
                v.incIt();
            }
        };

        Runnable r2 = new Runnable() {
            public void run() {
                v.doubleIt();
            }
        };

        test(v, sleepingTime, r1, r2);
    }

    // Same as zero, but with synchronization; yields 2
    private static void syncZero() {

        final IntVar v = new IntVar();

        // You might want to adjust this value
        long sleepingTime = 1;

        Runnable r1 = new Runnable() {
            public void run() {
                v.syncIncIt();
            }
        };

        Runnable r2 = new Runnable() {
            public void run() {
                v.syncDoubleIt();
            }
        };

        test(v, sleepingTime, r1, r2);
    }

    public static void main(String[] args) {
        zero();
        one();
        two();
        syncZero();
    }

}
