%%%-------------------------------------------------------------------
%%% @author Tilo
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%% @todo: remove keyboard process...
%%% @end
%%% Created : 12. Jan 2018 20:19
%%%-------------------------------------------------------------------
-module(chat).
-author("Tilo").

%% API
-export([start/2,start_server/0]).
-import(linda, [new_space/0,in/3,out/3,rd/3]).
-import(base, [getLn/1,printLn/1,show/1]).

start_server() ->
  Server = new_space(),
  out(Server, {loggedin, []}, -1),
  register(lindaspace, Server),
  Server.

keyboard(Name, Client) ->
  receive
    destroy -> ok
  after
    0 -> Input = getLn(Name++": "),
      Client ! {input,Input},
      keyboard(Name, Client)
  end.


start(Name,Node) ->
  {lindaspace,Node} ! {getPID,self()},
  S = receive
        {server, Server} -> Server
      end,
  Clients = in(S, fun({loggedin, X}) -> X end, -1),
  printLn(show(Clients) ++ " are already logged in!"),
  out(S, {message, Name ++ " logged in!", [self()]}, 10000000),
  out(S, {loggedin, [Name|Clients]}, -1),
  Me = self(),
  Keyboard = spawn_link(fun() -> keyboard(Name, Me) end),
  spawn_link(fun() -> receiving_loop(Me, S) end),
  loop(Name, S, Keyboard).

receiving_loop(Client, S) ->
  {Message, ClientsRead} =
    in(S, fun({message, Msg, ClientRead}) -> decide(Client, ClientRead, Msg) end, -1),
  Client ! {message, Message},
  out(S, {message, Message, [Client| ClientsRead]}, 10000000),
  receiving_loop(Client, S).

decide(Client, ClientsRead, Msg) ->
  case lists:member(Client, ClientsRead) of
    true  -> {'EXIT', "Tuple already read"};
    false -> {Msg, ClientsRead}
  end.

loop(Name, S, Keyboard) ->
  receive
    {input, Input} -> case Input of
                        ":q" -> Clients = in(S, fun({loggedin, X}) -> X end, -1),
                          NewClients = lists:delete(Name,Clients),
                          out(S,{loggedin, NewClients}, -1),
                          Keyboard ! destroy;
                        _ -> out(S, {message, Name ++ "\: " ++ Input, [self()]}, 10000000),
                          loop(Name, S, Keyboard)
                      end;
    {message, Message} -> printLn(Message),
      loop(Name, S, Keyboard)
  end.

