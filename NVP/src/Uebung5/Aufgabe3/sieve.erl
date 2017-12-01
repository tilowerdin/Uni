-module(sieve).
-export([firstNPrimes/1]).

% all numbers starting from N
allNumbers(N) ->
  receive
    {next,P} -> P!{num,N},
      allNumbers(N+1)
  end.

% ask previous sieve for next prime number
% start a new sieve process for next prime number
sieve(Prev) ->
  Prev!{next,self()},
  receive
    {num,N} -> receive
                 {prim,Collector} ->
                   Me = self(),
                   Child = spawn(fun() -> sieve(Me) end),
                   Collector!{prim,N,Child},
                   filter(N,Prev)
               end
  end.

% filter numbers with given prime number and
% send result to previous sieve process
filter(Prim,Prev) ->
  receive
    {next,P} -> P!{num,getNext(Prim,Prev)},
      filter(Prim,Prev)
  end.

% compute next prime number
getNext(Prim,Prev) ->
  Prev!{next,self()},
  receive
    {num,N} -> case (N rem Prim) of
                 0 -> getNext(Prim,Prev);
                 _ -> N
               end
  end.

% initialize collector
primes() ->
  AllNumbers = spawn(fun() -> allNumbers(2) end),
  Sieve = spawn(fun() -> sieve(AllNumbers) end),
  collector(Sieve).

% collect prime numbers
collector(Sieve) ->
  receive
    {nextPrim,P} -> Sieve!{prim,self()},
      receive
        {prim,N,NextSieve} -> P!{prim,N},
          collector(NextSieve)
      end
  end.

% return first N prim numbers
firstNPrimes(N) ->
  P = spawn(fun() -> primes() end),
  primeLoop(P,N).

primeLoop(_,0) -> [];
primeLoop(P,N) ->
  P!{nextPrim,self()},
  receive
    {prim, Prim} -> [Prim|primeLoop(P,N-1)]
  end.
