distChan.tar                                                                                        0000664 0001750 0001750 00000000000 13207547520 011446  0                                                                                                    ustar   fhu                             fhu                                                                                                                                                                                                                    kv.erl                                                                                              0000664 0001750 0001750 00000000520 13204756624 010340  0                                                                                                    ustar   fhu                             fhu                                                                                                                                                                                                                    -module(kv).
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
                                                                                                                                                                                kv_server.erl                                                                                       0000664 0001750 0001750 00000003674 13204762252 011735  0                                                                                                    ustar   fhu                             fhu                                                                                                                                                                                                                    -module(kv_server).
-export([server/0,client/0]).

server() ->
    {ok, LSock} = gen_tcp:listen(65065, [list, {packet, line}, 
                                         {reuseaddr, true}]),
    KV_store = spawn(fun() -> kv:kv_server() end),
    accept_loop(LSock,KV_store).

accept_loop(LSock,KV_store) ->
    Me = self(),
    spawn(fun() -> request_handler(LSock,KV_store,Me) end),
    receive
      next -> accept_loop(LSock,KV_store)
    end.

request_handler(LSock,KV_store,Parent) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    Parent ! next,
    receive
      {tcp,Sock,Str} ->
         case lists:splitwith(fun(C) -> C/=44 end, lists:delete(10,Str)) of
           {"store",[_|Arg]} ->
             {Key,[_|Value]} = lists:splitwith(fun(C) -> C/= 44 end,Arg),
             KV_store ! {store,Key,Value},
             base:printLn({store,Key,Value});
           {"lookup",[_|Key]} ->
             KV_store ! {lookup,Key,self()},
             base:printLn({lookup,Key}),
             receive
               {just,Ans} -> gen_tcp:send(Sock,"{just,"++Ans++"}\n");
               nothing    -> gen_tcp:send(Sock,"nothing\n")
             end
         end
    end,
    gen_tcp:close(Sock).

client() ->
  case base:getLn("(s)tore, (l)ookup: ") of
    "s" -> Key   = base:getLn("Key:   "),
           Value = base:getLn("Value: "),
           {ok,Sock} = gen_tcp:connect("localhost",65065,[list,{packet,line}]),
           gen_tcp:send(Sock,"store,"++Key++","++Value++"\n"),
           gen_tcp:close(Sock);
    "l" -> Key   = base:getLn("Key:   "),
           {ok,Sock} = gen_tcp:connect("localhost",65065,[list,{packet,line},
                                                          {active,false}]),
           gen_tcp:send(Sock,"lookup,"++Key++"\n"),
           case gen_tcp:recv(Sock,0) of
             {ok,Res} -> base:printLn(Res);
             Other -> base:printLn("Other TCP message: "++base:show(Other))
           end,
           gen_tcp:close(Sock)
   end.













                                                                    pingpong.erl                                                                                        0000664 0001750 0001750 00000001706 13207216221 011533  0                                                                                                    ustar   fhu                             fhu                                                                                                                                                                                                                    -module(pingpong).
-export([ping/0,start_ping/0]).
-import(remote_chan,[new_chan/0,chan_register/3,chan_lookup/2,read_chan/1,
                     write_chan/2,serialize/1]).

start_ping() -> Chan = new_chan(),
                chan_register("localhost","ping",Chan),
                ping_loop(Chan,0).

ping_loop(Chan,N) -> 
  case read_chan(Chan) of
    {ping,Ans_chan} -> write_chan(Ans_chan,{pong,N}),
                       ping_loop(Chan,N+1)
  end.

ping() ->
  case chan_lookup("localhost","ping") of
    nothing -> base:printLn("ping-service not available");
    {just,Chan} -> Ans_chan = new_chan(),
                   write_chan(Chan,{ping,Ans_chan}),
                   case read_chan(Ans_chan) of
                     {pong,N} -> {pong,N};
                     Oth  -> base:printLn("Unknown readChan result: "++base:show(Oth))  
                   end;
    Oth  -> base:printLn("Unknown chanLookup result: "++base:show(Oth))  
  end.
                   
                                                          remote_chan.erl                                                                                     0000664 0001750 0001750 00000005411 13207220312 012167  0                                                                                                    ustar   fhu                             fhu                                                                                                                                                                                                                    -module(remote_chan).
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























                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       