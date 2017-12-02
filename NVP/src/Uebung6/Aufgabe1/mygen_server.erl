%% @author Tilo
%% @doc @todo Add description to mygen_server.


-module(mygen_server).

-callback init(Identifier) -> Result when
	Identifier :: term(),
	Result :: {ok, State},
	State :: term().

-callback handle_call(Request, From, State) -> Answer when
	Request :: term(),
	From :: pid(),
	State :: term(),
	Answer :: {reply, Reply, NewState},
	Reply :: term(),
	NewState :: term().

-callback handle_cast(Request, From, State) -> Answer when
	Request :: term(),
	From :: pid(),
	State :: term(),
	Answer :: {noreply, NewState},
	NewState :: term().

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/2, call/2, cast/2]).

start(Modul, Identifier) -> {ok, State} = Modul:init(Identifier),
							spawn(fun() -> loop(Modul, State) end).

call(Gen_Server,Request) -> Gen_Server ! {mygen_server_call, self(), Request},
							receive
								Reply -> Reply
							end.

cast(Gen_Server,Request) -> Gen_Server ! {mygen_server_cast, self(), Request}.
								

%% ====================================================================
%% Internal functions
%% ====================================================================

loop(Modul, State) ->
	receive
		{mygen_server_call, Pid, Request} -> {replay, Reply, NewState} = Modul:handle_call(Request, Pid, State),
										     Pid ! Reply,
											 loop(Modul, NewState);
		{mygen_server_cast, Pid, Request} -> {noreply, NewState} = Modul:handle_cast(Request, Pid, State),
											 loop(Modul, NewState)
	end.
