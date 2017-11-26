-module (game_client).
-export ([start/2]).

start(Node, Name) ->
    Me = self(),
    Gui = game_gui:start(Me, 1000, 500),
    {game_server,Node} ! {login, Me, Name},
    receive
        {loggedin, SID} -> 
            io:fwrite("logged in~n"),
            start_(Gui, SID)
    end.

start_(Gui,SID) ->
    receive
        {text_update, Text} -> 
            io:fwrite("text"),
            Gui ! {text_update, Text},
            start_(Gui,SID);
        {button_create,ID,X,Y} ->
            io:fwrite("button " ++ integer_to_list(X) ++ " " ++ integer_to_list(Y)),
            Gui ! {button_create, ID, X, Y},
            start_(Gui,SID);
        button_remove ->
            io:fwrite("remove"),
            Gui ! button_remove,
            start_(Gui,SID);
        {clicked, ID} ->
            io:fwrite("clicked"),
            SID ! {clicked, ID, self()},
            start_(Gui, SID);
        close ->
            io:fwrite("close"),
            SID ! {logout, self()}
    end.
