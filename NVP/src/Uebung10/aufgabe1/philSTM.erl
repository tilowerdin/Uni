-module(philSTM).
-export([new_stick/0,take/1,put/1,start_phils/1]).
-import(stm,[new_tvar/1,read_tvar/1,write_tvar/2,retry/0,atomically/1]).

take(S) ->
  V = read_tvar(S),
  case V of
    true -> write_tvar(S,false);
    false -> retry()
  end.

put(S) ->
  write_tvar(S,true).

new_stick() -> atomically(fun() -> new_tvar(true) end).

phil(SL,SR,N) -> 
  base:printLn(base:show(N)++" is thinking."),
  atomically(fun() -> take(SL) end),
  atomically(fun() -> take(SR)
             end),
  base:printLn(base:show(N)++" is eating."),
  atomically(fun() -> put(SR),
                      put(SL)
             end),
  phil(SL,SR,N).

start_phils(N) -> 
  Sticks = create_sticks(N),
  Last_stick = create_phils(Sticks,2),
  base:getLn("Start last phil >"),
  phil(Last_stick,hd(Sticks),1).

create_sticks(0) -> [];
create_sticks(N) -> [new_stick() | create_sticks(N-1)].

create_phils([S1,S2|Sticks],N) ->
  spawn(fun() -> phil(S1,S2,N) end),
  create_phils([S2|Sticks],N+1);
create_phils([Last_stick],_) -> Last_stick.

















