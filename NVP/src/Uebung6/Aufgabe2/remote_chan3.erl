-module(remote_chan3).
-export([new_chan/0, read_chan/1, write_chan/3, register_chan/3,
         chan_lookup/2]).

new_chan() ->
	{local_chan, spawn(fun() -> chan() end)}.

chan() -> receive
            {read,P} -> receive
                          {write,V} -> P!{read_ans,V},
                                       chan()
                        end
          end.

register_chan(RegistryHost, Name, Chan) ->
	case lists:member(port_listener, registered()) of
		true -> ok;
        _ -> start_port_listener()
	end,
	case register_at_port(Name, Chan) of
        {ok, Host, Port} -> chan_reg(RegistryHost, Name, {remote_chan,Host, Port});
        Else -> Else
	end.
		
start_port_listener() ->
	{ok, LSock} = gen_tcp:listen(0, [list, {packet, line}, {reuseaddr,true}]),
	KV_Store = spawn(fun() -> kv:kv_server() end),
	{ok, Port} = inet:port(LSock),
	spawn(fun() -> register(port_listener, self()), send_to_port_listener(localhost, Port) end),
	spawn(fun() -> port_listener(LSock, KV_Store) end).

send_to_port_listener(Host, Port) ->
	receive
		{register, Name, Chan, Pid} -> {ok, Sock} = gen_tcp:connect(Host, Port, [list, {packet, line}, {active, false}]),
									   gen_tcp:send(Sock, serialize({register, Name, Chan}) ++ "\n"),
									   {ok, Str} = gen_tcp:recv(Sock, 0),
									   Pid ! deserialize(Str)
	end,
	send_to_port_listener(Host, Port).
			 

port_listener(LSock,KV_store) ->
  case gen_tcp:accept(LSock) of
    {ok,Sock} ->
      receive
        {tcp,_sock,MsgStr} ->
           {remote_msg,Msg} = deserialize(MsgStr),
		   case Msg of
			   {chanMessage, ChanName, Message} -> KV_store ! {lookup, ChanName, self()},
												   receive 
													   nothing -> ok;
													   {just,Chan} -> write_chan(Chan, Message)
												   end;
			   {register, Name, Chan} -> case kv:tryStore(KV_store, Name, Chan) of
										   ok -> gen_tcp:send(Sock, serialize({ok, localhost, inet:port(LSock)}) ++ "\n");
									       error -> gen_tcp:send(Sock, serialize(error) ++ "\n")
								         end
		   end,
           gen_tcp:close(Sock),
           port_listener(LSock,KV_store)
      end
  end.

register_at_port(Name, Chan) ->
	{port_listener, node()} ! {register, Name, Chan, self()},
	receive
		Result -> Result
	end.

chan_reg(_, _, {local_chan, _}) -> {error, "You need a remote chan here"};
chan_reg(Host,Name,Chan) ->
  {ok,Sock} = gen_tcp:connect(Host,65065,[list,{packet,line}]),
  gen_tcp:send(Sock,"store,"++Name++","++serialize(Chan)++"\n"),
  gen_tcp:close(Sock).


chan_lookup(Host,Name) -> 
  {ok,Sock} = gen_tcp:connect(Host,65065,[list,{packet,line},
                                          {active,false}]),
  gen_tcp:send(Sock,"lookup,"++Name++"\n"),
  Res = case gen_tcp:recv(Sock,0) of
          {ok,Str} -> deserialize(Str);
          Other -> base:printLn("Other Msg from kv_server: "++base:show(Other)),
                   invalid_kv_server_msg
        end,
  gen_tcp:close(Sock),
  Res. 

read_chan({local_chan,Ch}) -> 
                 Ch!{read,self()},
                 receive
                   {read_ans,V} -> V
                 end.

write_chan({local_chan,Ch},Msg) -> Ch!{write,Msg}.

write_chan({remote_chan,Host,Port}, ChanName ,Msg) ->
  case gen_tcp:connect(Host,Port,[list,{packet,line},{active,false}]) of
    {ok,Sock} ->
      gen_tcp:send(Sock,serialize({remote_msg,{chanMessage, ChanName, Msg}})++"\n"),
      gen_tcp:close(Sock)
  end.

serialize({local_chan,_}) -> {error, "this is not allowed anymore"};
serialize(X) when is_atom(X) -> atom_to_list(X);
serialize(X) when is_integer(X) -> integer_to_list(X);
%serialize(X) when is_pid(X) -> pid_to_list(X);
serialize(X) when is_tuple(X) -> "{"++serializeList(tuple_to_list(X))++"}";
serialize(X) when is_list(X) -> "["++serializeList(X)++"]".

serializeList([]) -> "";
serializeList([X|Xs]) -> serialize(X)++case Xs of
                                         [] -> "";
                                         _ -> ","++serializeList(Xs)
                                       end.

deserialize(Str) ->
  {ok,Tokens,_EndLine} = erl_scan:string(Str++"."),
  {ok,AbsForm} = erl_parse:parse_exprs(Tokens),
  {value,Value,_Bs} = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  Value.























