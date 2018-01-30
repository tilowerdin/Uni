-module(stm).
-export([atomically/1, new_tvar/1, read_tvar/1, write_tvar/2, retry/0]).
-import(gb_trees,[lookup/2,empty/0,enter/3,insert/3,keys/1,to_list/1]).

new_tvar(Value) -> spawn(fun() -> tvar(Value,0,[]) end).

tvar(Value,Version,Susps) ->
  receive
    {read,Pid} -> Pid!{value, Value, Version},
                  tvar(Value,Version,Susps);
    {lock,Pid} -> Pid ! locked,
                  tvar_locked(Value,Version,Susps)    
  end.

tvar_locked(Value,Version,Susps) ->
  receive
    {read,Pid}        -> Pid!{value, Value, Version},
                         tvar_locked(Value,Version,Susps);
    {write,New_value} -> lists:map(fun(Susp) -> Susp!{modified,self()} end,
                                   Susps),
                         tvar_locked(New_value,Version+1,[]);
    {newSusp,Susp,MyVersion} ->
      case MyVersion of
        Version -> Susp!susp,
          tvar_locked(Value,Version,[Susp|Susps]);
        _ -> Susp!{modified,self()},
          tvar_locked(Value,Version, Susps)
      end;
    unlock            -> tvar(Value,Version,Susps)
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

susp(TVar,P,MyVersion) ->
  TVar!{newSusp,P,MyVersion},
  receive
    susp -> susp;
    {modified,Pid} -> P!{modified,Pid}
  end.

read_tvar(TVar) ->
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
  {RS,WS} = get(state),
  put(state,{RS,enter(TVar,Value,WS)}),
  ok.

retry() -> throw(retry).
  
atomically(Transaction) ->
  put(state,{empty(),empty()}),
  case catch Transaction() of
    rollback -> atomically(Transaction);
    retry    -> {RS,_} = get(state),
                TVars = keys(RS),
                susps_l(to_list(RS)),
                receive
                  {modified,_TVar} -> ok
                end,
                atomically(Transaction);
    Res      -> {RS,WS} = get(state),
                TVars = lists:umerge(keys(RS),keys(WS)),
		            lock_l(TVars),
                case validate(to_list(RS)) of
                  true -> commit(to_list(WS)),
                          unlock_l(TVars),
                          Res;
                  false -> unlock_l(TVars),
                           atomically(Transaction)
                end
  end.

lock_l(TVars) -> lists:map(fun(TVar) -> lock(TVar) end, TVars).

unlock_l(TVars) -> lists:map(fun(TVar) -> unlock(TVar) end, TVars).

susps_l(TVars) -> Me = self(),
                  P = spawn(fun() -> 
                              receive
                               {modified,TVar} -> Me!{modified,TVar}
                              end end), 
                  lists:map(fun({TVar,Version}) -> susp(TVar,P,Version) end, TVars).

validate([]) -> true;
validate([{TVar,Version}|RS]) ->
  {value,_,Act_version} = core_read(TVar),
  Act_version==Version andalso validate(RS).

commit(WS) -> lists:map(fun({TVar,V}) -> core_write(TVar,V) end, WS).




