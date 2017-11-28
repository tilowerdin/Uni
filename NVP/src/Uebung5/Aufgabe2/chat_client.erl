-module(chat_client).
-export([join/2]).
-import(base,[print/1,printLn/1,getLn/1]).

join(Name,Node) ->
  {chat_server,Node} ! {login,Name,self()},
  receive
    {loggedin,Server,Other_users} ->
       base:printLn(Other_users),
       Me = self(),
       spawn_link(fun() -> keyboard(Name, Server, Me) end),
       client_loop()
  after
    2000 -> printLn("No contact to server possible.")
  end.

client_loop() ->
  receive
    {login,Name} -> printLn(Name++" joined."),
                    client_loop();
    {logout,Name} -> printLn(Name++" left."),
                     client_loop();
    {message,Name,Msg} -> printLn(Name++"> "++Msg),
                          client_loop()
  end.

keyboard(Name, Server, Client) ->
  Input = getLn(Name++": "),
  case Input of
    "bye." -> Server!{logout,Client},
             exit(-1);
    _     -> Server!{message,Client,Input},
             keyboard(Name, Server, Client)
  end.
