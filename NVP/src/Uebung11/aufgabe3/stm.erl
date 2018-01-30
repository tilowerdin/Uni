-module(stm).
-export([atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0]).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1]).

new_tvar(Value) -> spawn(fun() -> tvar(Value,0,[]) end).

tvar(Value,Version,Susps) ->
  receive
    {lock,Pid} -> Pid ! locked,
                  tvar_locked(Value,Version,Susps);
    {is_locked, Pid} -> Pid ! unlocked,
                        tvar(Value,Version,Susps)
  end.

tvar_locked(Value,Version,Susps) ->
  receive
    {read,Pid}        -> Pid!{value, Value, Version},
                         tvar_locked(Value,Version,Susps);
    {write,New_value} -> lists:map(fun(Susp) -> Susp!{modified,self()} end,
                                   Susps),
                         tvar_locked(New_value,Version+1,[]);
    {newSusp,Susp}    -> tvar_locked(Value,Version,[Susp|Susps]);
    unlock            -> tvar(Value,Version,Susps);
    {is_locked, Pid} -> Pid ! locked,
                        tvar(Value,Version,Susps)
end.

core_read(TVar) ->
  TVar!{read,self()},
  receive
    {value,V,Version} -> {value,V,Version}
  end.

core_write(TVar,Value) -> TVar!{write,Value}.

lock(TVar) ->
  TVar!{lock,self()},
  receive
    locked -> locked
  end.

unlock(TVar) -> TVar!unlock.

is_locked(TVar) ->
  TVar!{is_locked, self()},
  receive
    locked -> true;
    unlocked -> false
  end.

susp(TVar,P) -> TVar!{newSusp,P}. 

read_tvar(TVar) ->
  lock_or_rollback(TVar),
  {RS,WS} = get(state),
  case lookup(TVar,WS) of
    none      -> {value,V,Version} = core_read(TVar),
                 case lookup(TVar,RS) of
                   none            -> put(state,{insert(TVar,Version,RS),WS}),
                                      V;
                   {value,Version} -> V;
                   _               -> throw(rollback)
                 end;
    {value,V} -> V
  end. 

write_tvar(TVar,Value) ->
  lock_or_rollback(TVar),
  {RS,WS} = get(state),
  put(state,{RS,enter(TVar,Value,WS)}),
  ok.

retry() -> throw(retry).
  
atomically(Transaction) ->
  put(state,{empty(),empty()}),
  case catch Transaction() of
    rollback -> {RS,WS} = get(state),
                TVars = lists:umerge(keys(RS), keys(WS)),
                unlock_l(TVars),
                atomically(Transaction);
    retry    -> {RS,WS} = get(state),
                R_tvars = keys(RS),
                TVars = lists:umerge(R_tvars, keys(WS)),
                susps_l(R_tvars),
                unlock_l(TVars),
                receive
                  {modified,_TVar} -> ok
                end,
                atomically(Transaction);
    Res      -> {RS,WS} = get(state),
                TVars = lists:umerge(keys(RS),keys(WS)),
                commit(to_list(WS)),
                unlock_l(TVars),
                Res
  end.

unlock_l(TVars) -> lists:map(fun(TVar) -> unlock(TVar) end, TVars).

susps_l(TVars) -> Me = self(),
                  P = spawn(fun() -> 
                              receive
                               {modified,TVar} -> Me!{modified,TVar}
                              end end), 
                  lists:map(fun(TVar) -> susp(TVar,P) end, TVars).

commit(WS) -> lists:map(fun({TVar,V}) -> core_write(TVar,V) end, WS).

lock_or_rollback(TVar) ->
  {RS, WS} = get(state),
  TVars = lists:umerge(keys(RS), keys(WS)),
  case lists:member(TVar, TVars) of
    true -> true;
    false -> case is_locked(TVar) of
               true -> try_lock(TVar,1000);
               false -> lock(TVar),
                 true
             end
  end.

try_lock(TVar,TimeOut) ->
  Me = self(),
  spawn(fun() -> try_lock_helper(TVar,TimeOut,Me) end),
  receive
    lock -> true;
    no_lock -> throw(rollback)
  end.

try_lock_helper(TVar, TimeOut, Pid) ->
  TVar!{lock,self()},
  receive
    locked -> Pid!lock
  after
    TimeOut -> Pid!no_lock,
               receive
                 locked -> TVar!unlock
               end
  end.
