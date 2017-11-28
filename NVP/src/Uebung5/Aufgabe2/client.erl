-module (client).
-export ([start/3]).

start(Host, Port, Name) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, 
                                              {packet, 0}, 
                                              {active, false}]),
    sendMessage(Sock, {myName, Name}),
    Me = self(),
    Keyboard = spawn(fun() -> keyboard(Name, Me) end),
    link(Keyboard),
    Messages = spawn(fun() -> messageReceive(Sock, Me) end),
    link(Messages),
    clientLoop(Sock).

clientLoop(Sock) ->
    receive
        {welcome, Clients} -> base:printLn("already in chatroom: " ++ base:show(Clients)),
                              clientLoop(Sock);
        {ownInput, Msg} -> case Msg of
                               ":q" -> sendMessage(Sock, logout),
                                       exit(unnormal);
                               _ -> sendMessage(Sock, {message, Msg}),
                                    clientLoop(Sock)
                           end;
        {login, Name} -> base:printLn(Name ++ " joined."),
                         clientLoop(Sock);
        {logout, Name} -> base:printLn(Name ++ " left."),
                          clientLoop(Sock);
        {message, Name, Msg} -> base:printLn(Name ++ "> " ++ Msg),
                                clientLoop(Sock);
        {error, closed} -> base:printLn("connection lost"),
                           exit(unnormal)
    end.

messageReceive(Sock, ClientLoop) ->
    case tcpReceive(Sock) of
        {error, closed} -> ClientLoop ! {error, closed}, gen_tcp:close(Sock), exit(unnormal);
        {socket, Message} -> 
            ClientLoop ! Message,
            messageReceive(Sock, ClientLoop)
    end.

keyboard(Name, ClientLoop) ->
  Input = base:getLn(Name++": "),
  ClientLoop ! {ownInput, Input},
  keyboard(Name, ClientLoop).

sendMessage(Sock, Message) ->
    gen_tcp:send(Sock, term_to_binary({socket, Message})).


tcpReceive(Sock) ->
    case gen_tcp:recv(Sock, 0) of
        {error, closed} -> {error, closed};
        {error, Else} -> base:printLn(Else);
        {ok, B} -> binary_to_term(B)
    end.