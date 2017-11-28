-module (chat).
-export ([start/1,join/2]).

hashRange() ->
    power(2,32).

power(_,0) -> 1;
power(B,P) -> B * power(B, P-1).


setCookie() ->
    erlang:set_cookie(node(), chatErlang2).

start(Name) ->
    setCookie(),
    configure_(Name, [{self(),Name}]).

join(Name,Node) ->
    setCookie(),
    {chat,Node}!{login,self()},
    receive
        {loggedin,Clients} ->
            base:print("already in chat: "),
            base:printLn(lists:map(fun({_,CName}) -> CName end, Clients)), 
            link_all_Clients(Clients),
            NewClients = [{self(),Name} | Clients],
            broadcast(NewClients, {join,self(),Name,erlang:phash(NewClients,hashRange())}),
            configure_(Name, NewClients)
    after
        2000 ->
            base:printLn("Node not reachable.")
    end.

link_all_Clients(Clients) -> lists:map(fun({PID,_}) -> link(PID) end, Clients).

configure_(Name, Clients) ->
    try register(chat, self()) 
        catch error:badarg -> ok 
    end,
    process_flag(trap_exit, true),
    Me = self(),
    spawn_link(fun() -> link(Me), keyboard(Name, Me) end),
    loop(Clients).

loop(Clients) ->
    receive
        {login,CPID} ->
            CPID ! {loggedin, Clients},
            loop(Clients);
        {join,CPID,CName,Hash} ->
            base:printLn(CName ++ " joined."),
            NewClients = [{CPID,CName}|Clients],
             case erlang:phash(NewClients,hashRange()) of
                Hash -> loop(NewClients); % alles gut
                _ -> falseHash(CPID,NewClients)
            end;
        {ownInput,Msg} ->
            case Msg of
                ":q" ->
                    broadcast(Clients, {logout,self()});
                _ -> 
                    broadcast(
                            Clients, 
                            {message,self(),Msg,erlang:phash(Clients,hashRange())}),
                    loop(Clients)
            end;
        {message,PID,Msg,Hash} ->
            base:printLn(getName(PID,Clients) ++ "> " ++ Msg),
             case erlang:phash(Clients,hashRange()) of
                Hash -> loop(Clients);
                _ -> falseHash(PID,Clients)
            end;
        {logout,PID} ->
            base:printLn(getName(PID,Clients) ++ " left."),
            loop(lists:keydelete(PID,1,Clients));
        {'EXIT',PID,_} ->
            self()!{logout,PID},
            loop(Clients);
        {falseHash, CClients} ->
            case compareClients(Clients, CClients) of
                {equal, _} -> loop(CClients);
                {_, NewClients} -> 
                    broadcast(NewClients, {falseHash,NewClients}),
                    loop(NewClients)
            end;
        Msg -> 
            base:print("Unknown message: "),
            base:printLn(Msg),
            loop(Clients)
    end.

falseHash(CPID, Clients) ->
    base:printLn("false list of Clients"),
    CPID ! {falseHash, Clients},
    loop(Clients).

compareClients([], []) ->
    {equal,[]};
compareClients([], C2) ->
    {second, C2};
compareClients(C1, []) ->
    {first, C1};
compareClients([C1|C1s], C2s) ->
    case lists:member(C1,C2s) of
        true -> 
            {Which, Cs} = compareClients(C1s, lists:delete(C1,C2s)),
            {Which, [C1|Cs]};
        false ->
            case compareClients(C1s, C2s) of
                {equal, Cs} -> {first, [C1|Cs]};
                {second, Cs} -> {both, [C1|Cs]};
                {first, Cs} -> {first, [C1|Cs]};
                {both, Cs} -> {both, [C1|Cs]}
            end
    end.

getName(PID,Clients) ->
    case lists:keyfind(PID,1,Clients) of
        false -> "Unknown";
        {_,Name} -> Name
    end.

broadcast([],_Msg) -> ok;
broadcast([{PID,_Name}|Clients], Msg) ->
    Me = self(),
    case PID of
        Me -> broadcast(Clients, Msg);
        Other -> Other ! Msg, broadcast(Clients, Msg)
    end.

keyboard(Name, Loop) ->
    Input = base:getLn(Name++": "), 
    Loop ! {ownInput, Input},
    keyboard(Name, Loop).
