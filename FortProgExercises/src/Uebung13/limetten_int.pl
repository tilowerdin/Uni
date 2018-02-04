:- module(limetten_int, [limetten_int/1]).

:- use_module(library(clpfd)).

limetten_int(L) :-
    List = [PG,P],
    Einer = [E,T],
    Einer ins 0..9,
    List ins 0..2000,
     L - 99 #= PG,
      L #= T * 1000 + 200 + E,
      PG #= P*16.