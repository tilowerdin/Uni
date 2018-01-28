% Was ist eine Peano-Zahl?
isPeano(o).
isPeano(s(N)) :- isPeano(N).

% Nachfolger einer Peano-Zahl:
succ(N, s(N)).

% Vorgaenger einer Peano-Zahl:
pred(s(N), N).

% Addition auf Peano-Zahlen:
add(o,N,N) :- isPeano(N).
add(s(M),N,s(MN)) :- add(M,N,MN).

% Subtraktion auf Peano-Zahlen:
sub(X,Y,Z) :- add(Y,Z,X).

% Multiplikation auif Peano-Zahlen:
mult(o,_,o).
mult(s(M),N,P) :- mult(M,N,MN), add(MN,N,P).

% Kleiner-gleich:
leq(o,_).
leq(s(M),s(N)) :- leq(M,N).

allSafe([]).
allSafe([Q|Qs]) :- safe(Q,Qs,s(o)), allSafe(Qs).

% P ist der Linienabstand von Q und Q1
safe(_, []     , _).
safe(Q, [Q1|Qs], P) :- differentDiags(Q, Q1, P), safe(Q, Qs, s(P)).

% Unterschiedliche Diagonalen?
differentDiags(Q, Q1, P) :-
  add(Q1, P, Q1PP), Q \= Q1PP, % unterschiedliche Diagonale \
  add(Q , P, QPP ), QPP \= Q1. % unterschiedliche Diagonale /

queens(N,L) :-
  listOfAllN(N, NList),
  queensWithNList(NList, L),
  allSafe(L).
  
listOfAllN(o,[]).
listOfAllN(s(N),L) :-
  listOfAllN(N,L1),
  L = [s(N)|L1].

queensWithNList([],[]).
queensWithNList(NList, [E|L1]) :-
  member(E, NList),
  delete(NList, E, LOE),
  queensWithNList(LOE, L1).


% Anfrage für 1 liefert eine Lösung
% Anfrage für 2 liefert keine Lösung
% Anfrage für 3 liefert keine Lösung
% Anfrage für 4 liefert zwei Lösungen
% Anfrage für 5 liefert zehn Lösungen
% Anfrage für 6 liefert vier Lösungen

