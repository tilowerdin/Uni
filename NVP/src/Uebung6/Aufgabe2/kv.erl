-module(kv).
-export([kv_server/0]).

kv_server() -> kv_loop([]).

kv_loop(DB) ->
  receive
    {store,K,V}       -> kv_loop([{K,V}|DB]);
    {lookup,K,Client} -> Client ! lookup(K,DB),
                         kv_loop(DB) 
  end.


lookup(_,[])        -> nothing;
lookup(K,[{K,V}|_]) -> {just,V};
lookup(K,[_|KVs])   -> lookup(K,KVs).
