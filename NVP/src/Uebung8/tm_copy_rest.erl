-module (tm_copy_rest).
-export ([start_tm/2]).
-import (tm2, [move/3]).


start_tm(TM, Symb) -> run(q0, Symb, TM).

run(q0, _, TM)       -> case move(TM, right, b) of
                            b -> move(TM, left, b),
                                 Symb = move(TM, left, b),
                                 run(back, Symb, TM);
                            X -> move(TM, left, b),
                                 move(TM, right, X),
                                 run(q0, b, TM)
                        end;
run(back, b, TM)     -> move(TM, right, b);
run(back, X, TM)     -> Symb = move(TM, left, X),
                        run(back, Symb, TM).