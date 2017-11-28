-module(chat_server).
-export([start/0]).
-import(base,[printLn/1,show/1]).

start() -> register(chat_server,self()),
           process_flag(trap_exit,true),
           chat_server([]).

chat_server(Clients) ->
  receive
    {login,Name,CPid} ->
       link(CPid),
       broadcast(Clients,{login,Name}),
       CPid!{loggedin, self(), lists:map(fun({_,N}) -> N end, Clients)},
       chat_server([{CPid,Name}|Clients]);
    {logout,CPid} ->
        case lists:keyfind(CPid,1,Clients) of
          false -> chat_server(Clients);
          {_CPid,Name} -> NewClients = lists:keydelete(CPid,1,Clients),
                          broadcast(NewClients,{logout,Name}),
                          chat_server(NewClients)
        end;
    {message,CPid,Msg} ->
        case lists:keyfind(CPid,1,Clients) of
          false -> chat_server(Clients);
          {_CPid,Name} -> OtherClients = lists:keydelete(CPid,1,Clients),
                          broadcast(OtherClients,{message,Name,Msg}),
                          chat_server(Clients)
        end;
    {'EXIT',CPid,_} -> self()!{logout,CPid},
                       chat_server(Clients);
    Msg -> printLn("Unknown meesage: "++show(Msg)),
           chat_server(Clients)
  end.

broadcast(Clients,Msg) -> lists:map(fun({Pid,_}) -> Pid!Msg end, Clients).
