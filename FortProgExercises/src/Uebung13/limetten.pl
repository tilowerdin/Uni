:- module(limetten, [limetten/1]).

:- use_module(library(clpr)).
:- use_module(limetten_int).

limetten(L) :-
    { CT = L*100 }, 
    limetten_int(CT).