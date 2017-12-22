-module (tm).
-export ([start_tm/1]).

%% Zustand: {q, z, rl, ll}

next([])     -> {b, []};
next([X|XS]) -> {X, XS}.

start_tm(Input) -> {Z, List} = next(Input),
                   run({q0, Z, List, []}).

run({q0,b,_,_})    -> run({f,b,[],[]});
run({q0,$a,Rl,Ll}) -> {Z,XS} = next(Rl),
                      run({q1,Z,XS,Ll});
run({q1,$a,Rl,Ll}) -> {Z,XS} = next(Rl),
                      run({q1,Z,XS,[$a|Ll]});
run({q1,$b,Rl,Ll}) -> {Z,XS} = next(Rl),
                      run({q2,Z,XS,Ll});
run({q2,$b,Rl,Ll}) -> {Z,XS} = next(Rl),
                      run({q2,Z,XS,[$b|Ll]});
run({q2,$c,Rl,Ll}) -> {Z,XS} = next(Ll),
                      run({q3,Z,Rl,XS});
run({q3,b,Rl,Ll})  -> {Z,XS} = next(Rl),
                      run({q0,Z,XS,[b|Ll]});
run({q3,X,Rl,Ll})  -> {Z,XS} = next(Ll),
                      run({q3,Z,[X|Rl],XS});
run({f,_,_,_}) -> ok.





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% -------------------------- %%
%% bbbbbbbzeichenbbbbbbbbbbbb %%
%% -------------------------- %%
%%                            %%
%% -------------------------- %%
%% bbbbbbbbbbbbbbbbbbbbbbbbbb %%
%% -------------------------- %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%