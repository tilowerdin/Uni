-module (server).
-export ([start/1,start/0]).

start() ->
    start(8000).

start(Port) ->
    {ok, LSock} = gen_tcp:listen(Port, [binary, 
                                        {packet, 0}, 
                                        {active, false},
                                        {reuseaddr, true}]),
    ChatServer = spawn(fun() -> chatServerLoop([]) end),
    acceptLoop(LSock, ChatServer),
    gen_tcp:close(LSock).

acceptLoop(LSock,ChatServer) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> askForName(Sock, ChatServer) end),
    acceptLoop(LSock, ChatServer).

askForName(Sock, ChatServer) ->
    case tcpReceive(Sock) of
        {error, closed} -> ok;
        {socket,{myName, Name}} -> 
            ChatServer ! {newClient, Sock, Name, self()},
            receive
                newClientAccept -> messageReceive(Sock, ChatServer)
            after
                5000 -> 
                    sendMessage(Sock, {error, "something went wrong"})
            end
    end.

chatServerLoop(Clients) ->
    receive
        {newClient, Sock, Name, PID} ->
            PID ! newClientAccept,
            broadcast(Clients, {login, Name}),
            sendMessage(Sock, {welcome, lists:map(fun({_, CName}) -> CName end, Clients)}),
            chatServerLoop([{Sock, Name} | Clients]);
        {msg, Sock, logout} ->
            case lists:keyfind(Sock, 1, Clients) of
                false -> chatServerLoop(Clients);
                {_Sock, Name} ->
                    NewClients = lists:keydelete(Sock, 1, Clients),
                    broadcast(NewClients, {logout, Name}),
                    chatServerLoop(NewClients)
            end;
        {msg, Sock, {message, Msg}} ->
            case lists:keyfind(Sock, 1, Clients) of
                false -> ok;
                {_Sock, Name} ->
                    broadcast(
                        lists:keydelete(Sock, 1, Clients), 
                        {message, Name, Msg})
            end,
            chatServerLoop(Clients);
        {msg, _, Msg} ->
            base:print("Unknown message: "),
            base:printLn(Msg),
            chatServerLoop(Clients)
    end.

messageReceive(Sock, ChatServer) ->
    case tcpReceive(Sock) of
        {error, closed} -> ChatServer ! {logout, Sock};
        {socket, Message} -> 
            ChatServer ! {msg, Sock, Message},
            messageReceive(Sock, ChatServer)
    end.

broadcast(Clients, Msg) -> 
    lists:map(fun({Sock, Name}) -> base:printLn("sending " ++ Name ++ " a message"),
        sendMessage(Sock, Msg) end, Clients).

sendMessage(Sock, Message) ->
    gen_tcp:send(Sock, term_to_binary({socket, Message})).

tcpReceive(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {error, closed} -> {error, closed};
        {ok, B} -> binary_to_term(B)
    end.