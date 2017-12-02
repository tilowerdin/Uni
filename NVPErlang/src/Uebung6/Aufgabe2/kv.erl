-module(kv).
-export([kv_server/0,keymember/2,tryStore/3]).

kv_server() -> kv_loop([]).

kv_loop(DB) ->
  receive
    {store,K,V}       -> kv_loop([{K,V}|DB]);
    {lookup,K,Client} -> Client ! lookup(K,DB),
                         kv_loop(DB);
	{tryStore,K,V,Client} -> case lookup(K,DB) of
							     nothing -> NewDB = [{K,V} | DB],
							                Client ! {tryStore, ok},
							                kv_loop(NewDB);
								 {just, _} -> Client ! {tryStore, error},
											  kv_loop(DB)
							 end
  end.


lookup(_,[])        -> nothing;
lookup(K,[{K,V}|_]) -> {just,V};
lookup(K,[_|KVs])   -> lookup(K,KVs).

keymember(KV_store, Key) ->
	KV_store ! {lookup, Key, self()},
	receive
		Message -> case Message of
					   nothing -> false;
					   {just, V} -> {true, V}
				   end
	end.

tryStore(KV_store, Key, Value) ->
	KV_store ! {tryStore, Key, Value, self()},
	receive
		{tryStore, Result} -> Result
	end.