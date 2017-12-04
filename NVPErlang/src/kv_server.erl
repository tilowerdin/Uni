-module(kv_server).
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













