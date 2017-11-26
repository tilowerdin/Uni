-module (game_server).
-export ([start/0]).
-import (rand, [uniform/1]).

start() ->
    setUp().

setUp() ->
    register(game_server, self()),
    io:fwrite("registered"),
    process_flag(trap_exit, true),
    start_([],getNewButton(0)),
    unregister(game_server).

start_(Clients,{ID, X, Y}) ->
    receive
        {login, CPID, Name} ->
            io:fwrite("login " ++ Name),
            CPID ! {loggedin, self()},
            CPID ! {button_create,ID,X,Y},
            link(CPID),
            start_([{CPID,Name,0} | Clients], {ID, X, Y});
        {logout, CPID} ->
            io:fwrite("logout"),
            start_(lists:keydelete(CPID,1,Clients), {ID,X,Y});
        {clicked, BtnID, CPID} ->
            io:fwrite("clicked"),
            case BtnID == ID of
                false ->
                    start_(Clients,{ID,X,Y});
                true ->
                    case lists:keyfind(CPID,1,Clients) of
                        false ->
                            start_(Clients,{ID,X,Y});
                        {_CPID, Name, Points} ->
                            NewClients = [{CPID,Name,Points+1} | 
                                          lists:keydelete(CPID,1,Clients)],
                            broadcast(
                                NewClients,
                                {text_update,getStanding(NewClients)}),
                            {NBID,NX,NY} = getNewButton(ID+1),
                            broadcast(
                                NewClients,
                                {button_create,NBID,NX,NY}),
                            start_(NewClients,{NBID,NX,NY})
                    end
            end;
        {'EXIT', PID, _} ->
            io:fwrite("connection lost"),
            self() ! {logout, PID},
            start_(Clients,{ID,X,Y})
    end.

getStanding([]) ->
    "";
getStanding([{_, Name, Points} | Clients]) ->
    (Name ++ (": " ++ (integer_to_list(Points) ++ ("; " ++ 
        getStanding(Clients))))).

broadcast(Clients, Msg) -> lists:map(fun({PID,_,_}) -> PID ! Msg end, Clients).

getNewButton(ID) ->
    {ID, uniform(900),uniform(450)}.
%    {ID,0,0}.