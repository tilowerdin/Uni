spieler(wolff).
spieler(heinevetter).
spieler(drux).
spieler(kuehn).
spieler(weber).

verein(kiel).
verein(berlin).
verein(leipzig).
verein(melsungen).

position(rl).
position(tw).
position(rm).

spieltFuer(heinevetter, berlin).
spieltFuer(wolff, kiel).
spieltFuer(drux, berlin).
spieltFuer(kuehn, melsungen).
spieltFuer(weber, leipzig).

spieltPosition(heinevetter, tw).
spieltPosition(wolff, tw).
spieltPosition(drux, rl).
spieltPosition(kuehn, rl).
spieltPosition(weber, rm).

kannAuswechseln(Spieler1, Spieler2) :-
  spieler(Spieler1),
  spieler(Spieler2),
  Spieler1 \= Spieler2,
  spieltPosition(Spieler1, X),
  spieltPosition(Spieler2, X).

spielenZusammen(Spieler1, Spieler2) :-
  spieler(Spieler1),
  spieler(Spieler2),
  Spieler1 \= Spieler2,
  spieltFuer(Spieler1, X),
  spieltFuer(Spieler2, X).

