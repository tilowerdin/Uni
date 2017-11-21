-module (chat).
-export ([start/1,join/2]).

setCookie() ->
    erlang:set_cookie(node(), chatErlang1).

start(Name) ->
    setCookie(),
    configure(Name,[]).

join(Name,Node) ->
    setCookie(),
    {chat,Node}!{login,Name,self()},
    receive
        {loggedin,Clients} ->
            base:print("already in chat: "),
            base:printLn(lists:map(fun({_,CName}) -> CName end, Clients)), 
            link_all_Clients(Clients),
            configure(Name, Clients)
    after
        2000 ->
            base:printLn("Node not reachable.")
    end.

configure(Name,Clients) ->
    case lists:member(chat, registered()) of
        true -> 
            configure_(Name, Clients);
        _ ->
            register(chat, self()),
            configure_(Name,Clients),
            unregister(chat)
    end.

configure_(Name,Clients) ->
    process_flag(trap_exit, true),
    Me = self(),
    spawn_link(fun() -> link(Me), keyboard(Name, Me) end),
    loop(Name,Clients).

tryRegisterAndLoop(Name, Clients) ->
    case lists:member(chat, registered()) of
        true ->
            loop(Name, Clients);
        _ ->
            register(chat, self()),
            loop(Name, Clients),
            unregister(chat)
    end.

link_all_Clients(Clients) -> lists:map(fun({PID,_}) -> link(PID) end, Clients).

loop(Name, Clients) ->
    receive
        {login,ClientName,PID} ->
            base:printLn(ClientName ++ " joined"),
            broadcast(Clients, {join, ClientName, PID}),
            PID ! {loggedin, [{self(),Name}|Clients]},
            loop(Name, [{PID,ClientName}|Clients]);
        {join,ClientName,PID} ->
            base:printLn(ClientName ++ " joined"),
            loop(Name, [{PID,ClientName}|Clients]);
        {ownInput,Msg} ->
            case Msg of
                ":q" ->
                    broadcast(Clients, {logout, self()});
                _ -> 
                    broadcast(Clients, {message, Name,Msg}),
                    loop(Name, Clients)
            end;
        {message,CName,Msg} ->
            base:printLn(CName ++ "> " ++ Msg),
            loop(Name, Clients);
        {logout,PID} ->
            case lists:keyfind(PID,1,Clients) of
                false ->
                    loop(Name,Clients);
                {_PID,CName} ->
                    base:printLn(CName++" left."),
                    tryRegisterAndLoop(Name, lists:keydelete(PID,1,Clients))
            end;
        {'EXIT',PID,_} ->
            self()!{logout,PID},
            loop(Name,Clients);
        Msg -> base:print("Unknown message: "), base:printLn(Msg), loop(Name, Clients)
    end. 


broadcast([],_Msg) -> ok;
broadcast([{PID,_Name}|Clients], Msg) ->
    PID ! Msg,
   broadcast(Clients, Msg).
%broadcast(Clients, Msg) -> lists:map(fun({PID,_}) -> PID!Msg end, Clients).

keyboard(Name, Loop) ->
    Input = base:getLn(Name++": "), 
    Loop ! {ownInput, Input},
    keyboard(Name, Loop).
