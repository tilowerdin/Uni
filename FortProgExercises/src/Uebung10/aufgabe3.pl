mensch(meier).
mensch(mueller).
mensch(schroeder).
mensch(schulz).

in(X, [X|_]).
in(X, [_|L]) :- in(X, L).

gdw(X, Y) :-
  (X,Y);
  (not(X),not(Y)).

bed1(V,K,S) :- not((in(meier, [V,K,S]), in(mueller, [V,K,S]))).

bed2(V,K,S) :- not(in(mueller, [K,S])); V = schulz.

bed3(V,K,S) :- not(in(schroeder,[V,K,S])); in(meier, [V,K,S]).

bed4(V,K,S) :- not(in(meier, [V,K,S])); not(S = schulz).

bed5(V,K,S) :- not(in(schulz, [V,K,S])); not(V = schroeder).





doit(V, K, S) :-
  mensch(V),
  mensch(K),
  mensch(S),
  V \= K,
  V \= S,
  K \= S,
  bed1(V,K,S),
  bed2(V,K,S),
  bed3(V,K,S),
  bed4(V,K,S),
  bed5(V,K,S).

vorstand(V,K,S) :-
  setof((V,K,S), doit(V,K,S), Set),
  member((V,K,S), Set).