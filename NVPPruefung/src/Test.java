import java.util.ArrayList;

public class Test {

    public static void main(String[] args) throws InterruptedException {
/*        STM stm = new STM();
        STM.TVar<Integer> tvar = new STM.TVar<>(5);
        STM.Fun fun1 = new STM.Fun() {
            @Override
            public Object call() throws STM.RollbackException, STM.RetryException {
                stm.writeTVar(tvar, 5);
                return null;
            }
        };
        STM.Fun fun = new STM.Fun() {
            @Override
            public Integer call() throws STM.RollbackException {
                Integer value = stm.readTVar(tvar);
                stm.writeTVar(tvar, value+1);
                return value+1;
            }
        };

        System.out.println("1.");
        stm.atomically(fun1);
        System.out.println("2.");
        Object erg = stm.atomically(fun);

        System.out.println(erg);*/

        phils();
    }

    private static void phils() {
        int count = 5;
        ArrayList<STM.TVar<Boolean>> sticks = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            sticks.add(new STM.TVar<>(true));
        }
        ArrayList<Phil> phils = new ArrayList<>();
        for (int i = 0; i < count; i++) {
            phils.add(new Phil(i, sticks.get(i), sticks.get((i+1) % count)));
            phils.get(i).start();
            System.out.println("starting phil " + i);
        }
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
        }
        for (int i = 0; i < count; i++){
            phils.get(i).interrupt();
        }
        try {
            Thread.sleep(20);
        } catch (InterruptedException e) {
        }
        for (int i = 0; i < count; i++){
            System.out.println(i + " " + phils.get(i).getCount());
        }
    }

    private static void takeStick(STM stm, STM.TVar<Boolean> stick) throws STM.RetryException, STM.RollbackException {
        Boolean free = stm.readTVar(stick);
        if (free) {
            stm.writeTVar(stick, false);
        } else {
            stm.retry();
        }
    }

    private static void releaseStick(STM stm, STM.TVar<Boolean> stick) {
        stm.writeTVar(stick, true);
    }

    static class Phil extends Thread {

        private STM.TVar<Boolean> stickL;
        private STM.TVar<Boolean> stickR;
        private String id;
        private int count = 0;

        public Phil(int id, STM.TVar<Boolean> stickL, STM.TVar<Boolean> stickR) {
            this.stickL = stickL;
            this.stickR = stickR;
            this.id = "";
            for (int i = 0; i < id; i++){
                this.id += " ";
            }
        }

        @Override
        public void run() {
            try {
                STM stm = new STM();
                while (!Thread.interrupted()) {
                    count++;
                    stm.atomically(() -> {
                        takeStick(stm, stickL);
                        takeStick(stm, stickR);
                        return null;
                    });
//                    stm.atomically(() -> {takeStick(stm, stickL); return null;});
//                    stm.atomically(() -> {takeStick(stm, stickR); return null;});
                    System.out.println(id + "e");
                   // Thread.yield();
                    stm.atomically(() -> {
                        releaseStick(stm, stickR);
                        releaseStick(stm, stickL);
                        return null;
                    });
//                    stm.atomically(() -> {releaseStick(stm, stickL); return null;});
//                    stm.atomically(() -> {releaseStick(stm, stickR); return null;});
                    System.out.println(id + "t");
                //    Thread.yield();
                }
            } catch (InterruptedException e){
                System.out.println(id + count);
            }
        }

        public int getCount() {return count;}
    }

}
