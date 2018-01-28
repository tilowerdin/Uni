-module (stmtest).
-import (stmerw, [atomically/1, new_tvar/1, read_tvar/1, 
               write_tvar/2, retry/0, print_protocol/0]).
-export ([start_test/3,sum_tvars/1,create_tvars/1,
          create_threads/3, doStuff/3,doAtomically/2,retry_test/0]).

start_test(Ts, TVars, Apt) ->
  TVs = create_tvars(TVars),
  register(finish,self()),
  create_threads(Ts, TVs, Apt),
  waitForThreads(Ts),
  Sum = Apt * 200 * Ts,
  ActSum = atomically(fun() -> sum_tvars(TVs) end),
  case ActSum of
    Sum -> base:printLn("correct sum of TVars");
    _   -> base:printLn("false sum of TVars: should be " ++ 
                        base:show(Sum) ++ " was " ++ base:show(ActSum))
  end,
  unregister(finish).

retry_test() ->
  TVar = atomically(fun() -> new_tvar(0) end),
  spawn(fun() -> atomically(fun() -> doretry(TVar) end) end),
  receive
  after
    100 -> atomically(fun() -> write_tvar(TVar,1) end)
  end.

doretry(TVar) ->
  Value = read_tvar(TVar),
  case Value of
    0 -> base:printLn("retry"),
         retry();
    _ -> base:printLn("no retry")
  end.

waitForThreads(0) -> ok;
waitForThreads(N) -> 
  receive
    finished -> waitForThreads(N-1)
  end.

sum_tvars([]) -> 0;
sum_tvars([TVar|TVars]) ->
  read_tvar(TVar) + sum_tvars(TVars).

create_tvars(0) -> [];
create_tvars(TVars) -> 
  [atomically(fun() -> new_tvar(0) end)|create_tvars(TVars-1)].

create_threads(0, _, _) -> ok;
create_threads(Ts, TVars, Apt) ->
  spawn(fun() -> doStuff(200, TVars, Apt),
    %print_protocol(),
    finish!finished end),
  create_threads(Ts-1, TVars, Apt).

doStuff(0, _, _) -> ok;
doStuff(Count, TVars, Apt) ->
  atomically(fun() -> doAtomically(TVars, Apt) end),
  doStuff(Count-1, TVars, Apt).

doAtomically(_, 0) -> ok;
doAtomically(TVars, Apt) ->
  Index = random:uniform(length(TVars)),
  TVar = lists:nth(Index, TVars),
  V = read_tvar(TVar),
  write_tvar(TVar, V+1),
  I = random:uniform(100000000),
  case I < 20000 of
    true ->
      retry();
    _ -> 
      doAtomically(TVars, Apt-1)
  end.
