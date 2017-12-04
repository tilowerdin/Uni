%% @author Tilo
%% @doc @todo Add description to new_file.


-module(new_file).

%% ====================================================================
%% API functions
%% ====================================================================
-export([f/1, x/0]).

f(X) ->
	io:format("~p~n", [X]),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================

x() ->
    spawn(fun j/0),
    spawn(fun j/0),
    spawn_link(fun j/0),
    spawn_link(fun j/0),
    ok.

j() ->
    receive 
        after 4000 -> 
                ok 
    end.
