%%%-------------------------------------------------------------------
%%% @author Tilo
%%% @copyright (C) 2018, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2018 14:16
%%%-------------------------------------------------------------------
-module(test).
-author("Tilo").

%% API
-export([start/0,start_process/0,stop/1,link_leave/1]).

start_process() -> spawn(fun() -> start() end).

start() ->
  base:getLn("Hallo: "),
  Me = self(),
  spawn_stop(Me),
  start().

stop(PID) ->
  exit(PID, kill).

link_leave(PID) ->
  spawn(fun() -> link(PID) end).

spawn_stop(PID) ->
  spawn(fun() -> stop(PID) end).
