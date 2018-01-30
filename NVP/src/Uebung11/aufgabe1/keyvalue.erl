%%%-------------------------------------------------------------------
%%% @author Tilo
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Jan 2018 12:11
%%%-------------------------------------------------------------------
-module(keyvalue).
-author("Tilo").

%% API
-export([new_kv/0,insert/2,lookup/1,delete/1,retry/0,atomically/2]).

new_kv() ->
  spawn(fun() -> kv([]) end).

insert(Key,Value) ->
  {RS, WS} = get(state),
  put(state, {RS, gb_trees:enter(Key,Value,WS)}).

lookup(Key) ->
  {RS,WS} = get(state),
  KV_store = get(kv),
  case gb_trees:lookup(Key, WS) of
    none -> case core_lookup(KV_store, Key) of
               {value, Value, Version} -> case gb_trees:lookup(Key,RS) of
                                            none -> put(state, {gb_trees:enter(Key, Version, RS), WS}),
                                                    Value;
                                            {value,Version} -> Value;
                                            _ -> throw(rollback)
                                          end;
               {noSuchKey, Key} -> {noSuchKey, Key}
             end;
    {value, delete} -> {noSuchKey, Key};
    {value, Value} -> Value
  end.

delete(Key) ->
  {RS, WS} = get(state),
  put(state, {RS, gb_trees:enter(Key,delete,WS)}),
  ok.

retry() -> throw(retry).

atomically(Fun, KV_store) ->
  put(kv, KV_store),
  put(state, {gb_trees:empty(), gb_trees:empty()}),
  case catch Fun() of
    rollback -> atomically(Fun, KV_store);
    retry -> {RS, _} = get(state),
             Keys = gb_trees:keys(RS),
             lock_keys(Keys),
             case validate(gb_trees:to_list(RS)) of
               true -> susp_keys(Keys),
                       unlock_keys(Keys),
                       receive
                         modified -> ok
                       end;
               false -> unlock_keys(Keys)
             end,
             atomically(Fun, KV_store);
    Res -> {RS,WS} = get(state),
           Keys = lists:umerge(gb_trees:keys(RS), gb_trees:keys(WS)),
           lock_keys(Keys),
           case validate(gb_trees:to_list(RS)) of
             true -> commit(gb_trees:to_list(WS)),
                     unlock_keys(Keys),
                     Res;
             false -> unlock_keys(Keys),
                      atomically(Fun, KV_store)
           end
  end.

%% inner funtions

kv(DB) ->
  receive
    {lock, Key, Pid} ->
      case lists:keyfind(Key, 1, DB) of
        false -> Pid!{noSuchKey,Key},
          kv(DB);
        Entry -> Entry!{lock, self()},
          receive
            locked -> Pid ! locked
          end,
          kv(DB)
      end;
    {unlock, Key, Pid} ->
      case lists:keyfind(Key, 1, DB) of
        false -> Pid!{noSuchKey,Key},
          kv(DB);
        Entry -> Entry!unlock,
          Pid!unlocked,
          kv(DB)
      end;
    {insert, Key, Value} ->
      case lists:keyfind(Key, 1, DB) of
        false -> kv([{Key, new_DB_entry(Value)} | DB]);
        Entry -> Entry!{insert, Value},
          kv(DB)
      end;
    {lookup, Key, Pid} ->
      case lists:keyfind(Key, 1, DB) of
        false -> Pid!{noSuchKey,Key},
          kv(DB);
        Entry -> Entry!{lookup, self()},
          receive
            {value, Value, Version} -> Pid!{value,Value,Version}
          end,
          kv(DB)
      end;
    {delete, Key} ->
      case lists:keyfind(Key, 1, DB) of
        false -> kv(DB);
        Entry -> Entry!delete,
          kv(lists:keydelete(Key,1,DB))
      end;
    {newSusp, Key, Pid} ->
      case lists:keyfind(Key, 1, DB) of
        false -> Pid!{noSuchKey, Key};
        Entry -> Entry!{newSusp, Pid}
      end,
      kv(DB)
  end.

new_DB_entry(Value) ->
  spawn(fun() -> entry_unlocked(Value, 0, []) end).

entry_unlocked(Value, Version, Susps) ->
  receive
    {lookup, Pid} ->
      Pid!{value, Value, Version},
      entry_unlocked(Value, Version, Susps);
    {lock, Pid} ->
      Pid!locked,
      entry_locked(Value, Version, Susps)
  end.

entry_locked(Value, Version, Susps) ->
  receive
    {lookup, Pid} ->
      Pid!{value, Value, Version},
      entry_locked(Value, Version, Susps);
    unlock ->
      entry_unlocked(Value, Version, Susps);
    {insert, NewValue} ->
      lists:map(fun(Susp) -> Susp!modified end, Susps),
      entry_locked(NewValue, Version+1, []);
    delete ->
      lists:map(fun(Susp) -> Susp!modified end, Susps),
      ok;
    {newSusp,Susp} -> entry_locked(Value, Version, [Susp|Susps])
  end.

core_lookup(KV, Key) ->
  KV!{lookup, Key, self()},
  receive
    X -> X
  end.

core_delete(KV, Key) ->
  KV!{delete, Key}.

core_insert(KV, Key, Value) ->
  KV!{insert, Key, Value}.

lock(KV, Key) ->
  KV!{lock, Key, self()},
  receive
    X -> X
  end.

unlock(KV, Key) ->
  KV!{unlock, Key, self()},
  receive
    X -> X
  end.

susp(KV, Key, Pid) ->
  KV!{newSusp, Key, Pid}.

lock_keys(Keys) ->
  KV_store = get(kv),
  lists:map(fun(Key) -> lock(KV_store, Key) end, Keys).

validate([]) -> true;
validate([{Key,Version}|RS]) ->
  KV = get(kv),
  case core_lookup(KV, Key) of
    {value, _, Version} -> validate(RS);
    _ -> false
  end.

susp_keys(Keys) ->
  Me = self(),
  KV = get(kv),
  P = spawn(fun() -> receive
                       modified -> Me!modified
                     end end),
  lists:map(fun(Key) -> susp(KV, Key, P) end, Keys).

unlock_keys(Keys) ->
  KV_store = get(kv),
  lists:map(fun(Key) -> unlock(KV_store, Key) end, Keys).


commit(WS) ->
  KV = get(kv),
  lists:map(fun({Key, Value}) -> case Value of
                                   delete -> core_delete(KV, Key);
                                   _ -> core_insert(KV, Key, Value)
                                 end end, WS).