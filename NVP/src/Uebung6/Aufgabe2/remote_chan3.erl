-module(remote_chan3).
-export([new_chan/0, read_chan/1, write_chan/2,chan_register/3,
         chan_lookup/2]).

new_chan() -> 
  case gen_tcp:listen(0,[list,{packet,line},{reuseaddr,true}]) of
    {ok,LSock} -> {ok,Port} = inet:port(LSock),
                  Chan = {local_chan, spawn(fun() -> chan() end),
                          localhost, Port},
                  spawn(fun() -> port_listener(LSock,Chan) end),
                  Chan
  end.

port_listener(LSock,Chan) ->
  case gen_tcp:accept(LSock) of
    {ok,Sock} ->
      receive
        {tcp,_sock,MsgStr} ->
           {remote_msg,Msg} = deserialize(MsgStr),
           write_chan(Chan,Msg),
           gen_tcp:close(Sock),
           port_listener(LSock,Chan)
      end
  end.

chan_register(Host,Name,Chan) ->
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

chan() -> receive
            {read,P} -> receive
                          {write,V} -> P!{read_ans,V},
                                       chan()
                        end
          end.  

read_chan({local_chan,Ch,_,_}) -> 
                 Ch!{read,self()},
                 receive
                   {read_ans,V} -> V
                 end.

write_chan({local_chan,Ch,_,_},Msg) -> Ch!{write,Msg};
write_chan({remote_chan,Host,Port},Msg) ->
  case gen_tcp:connect(Host,Port,[list,{packet,line},{active,false}]) of
    {ok,Sock} ->
      gen_tcp:send(Sock,serialize({remote_msg,Msg})++"\n"),
      gen_tcp:close(Sock)
  end.

serialize({local_chan,_,Host,Port}) -> serialize({remote_chan,Host,Port});
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























