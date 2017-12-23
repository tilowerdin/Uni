-module (tm).
-export ([start_tm/1]).
-import (tm2, [new_tm/0, move/3, set_input/2]).

%% Zustand: {q, z, rl, ll}

start_tm(Input) -> TM = new_tm(),
                   Symb = set_input(TM, Input),
                   run(q0, Symb, TM).

run(q0,b,_)    -> run(f,b,[]);
run(q0,$a,TM)  -> Symb = move(TM, right, b),
                  run(q1, Symb, TM);
run(q1,$a,TM)  -> Symb = move(TM, right, $a),
                  run(q1, Symb, TM);
run(q1,$b,TM)  -> Symb = move(TM, right, b),
                  run(cb1, Symb, TM);
run(cb1,$b,TM) -> move(TM, left, b),
                  run(cb2, b, TM);
run(cb2,_,TM)  -> move(TM, right, $b),
                  Symb = move(TM, right, b),
                  run(cb1,Symb, TM);
run(cb1,$c,TM) -> move(TM, left, b),
                  run(cb3, b, TM);
run(cb3,_,TM)  -> move(TM, right, $c),
                  Symb = move(TM, right, b),
                  run(cb1, Symb, TM);
run(cb1,b,TM)  -> move(TM, left, b),
                  Symb = move(TM, left, b),
                  run(cb4, Symb, TM);
run(cb4,$c,TM) -> Symb = move(TM, left, $c),
                  run(cb4, Symb, TM);
run(cb4,X,TM)  -> Symb = move(TM, right, X),
                  run(q2, Symb, TM);
run(q2,$c,TM)  -> Symb = move(TM, right, b),
                  run(cc1, Symb, TM);
run(cc1,$c,TM) -> move(TM, left, b),
                  run(cc2, b, TM);
run(cc2,_,TM)  -> move(TM, right, $c),
                  Symb = move(TM, right, b),
                  run(cc1, Symb, TM);
run(cc1,b,TM)  -> Symb = move(TM, left, b),
                  run(qb, Symb, TM);
run(qb,b,TM)   -> Symb = move(TM, right, b),
                  run(q0, Symb, TM);
run(qb,X,TM)   -> Symb = move(TM, left, X),
                  run(qb, Symb, TM);
run(f,_,_) -> ok.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -------------------------- %%
%% bbbbbbbzeichenbbbbbbbbbbbb %%
%% -------------------------- %%
%%                            %%
%% -------------------------- %%
%% bbbbbbbbbbbbbbbbbbbbbbbbbb %%
%% -------------------------- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%