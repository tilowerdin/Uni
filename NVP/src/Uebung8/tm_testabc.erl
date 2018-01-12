-module (tm_testabc).
-export ([start_tm/2]).
-import (tm2, [move/3]).

start_tm(TM, Symb) -> run(q0, Symb, TM).

run(q0, b, _)     -> b;
run(q0, $a, TM)   -> Symb = move(TM, right, $a),
                     run(ra, Symb, TM);
run(ra, $a, TM)   -> Symb = move(TM, right, $a),
                     run(ra, Symb, TM);
run(ra, $b, TM)   -> Symb = move(TM, right, $b),
                     run(rb, Symb, TM);
run(rb, $b, TM)   -> Symb = move(TM, right, $b),
                     run(rb, Symb, TM);
run(rb, $c, TM)   -> Symb = move(TM, right, $c),
                     run(rc, Symb, TM);
run(rc, $c, TM)   -> Symb = move(TM, right, $c),
                     run(rc, Symb, TM);
run(rc, b, TM)    -> Symb = move(TM, left, b),
                     run(back, Symb, TM);
run(back, b, TM)  -> move(TM, right, b);
run(back, X, TM)  -> Symb = move(TM, left, X),
                     run(back, Symb, TM).