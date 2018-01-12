-module (tm).
-export ([start_tm/1]).
-import (tm2, [new_tm/0, move/3, set_input/2]).

%% Zustand: {q, z, rl, ll}

start_tm(Input) -> TM = new_tm(),
                   Symb = set_input(TM, Input),
                   run(qst, Symb, TM).

run(qst, b, TM) -> run(f, b, TM);
run(qst, $a, TM) -> Symb = tm_testabc:start_tm(TM, $a),
                    run(qsa, Symb, TM);
run(qsa, b, TM) -> run(f, b, TM);
run(qsa, $a, TM) -> Symb = move(TM, right, b),
                   run(qra, Symb, TM);
run(qra, $a, TM) -> Symb = move(TM, right, $a),
                    run(qra, Symb, TM);
run(qra, $b, TM) -> Symb = tm_copy_rest:start_tm(TM, $b),
                    run(qrb, Symb, TM);
run(qrb, $a, TM) -> Symb = move(TM, right, $a),
                    run(qrb, Symb, TM);
run(qrb, $b, TM) -> Symb = move(TM, right, $b),
                    run(qrb, Symb, TM);
run(qrb, $c, TM) -> Symb = tm_copy_rest:start_tm(TM, $c),
                    run(qsa, Symb, TM);
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