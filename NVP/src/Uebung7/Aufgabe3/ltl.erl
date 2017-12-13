-module(ltl).
-export([prop/1,neg/1,conj/2,disj/2,x/1,f/1,g/1,r/2,u/2,w/2,fn/2,
         start/0,
         assert/1,newProp/1,releaseProp/1,status/0]).

prop(Phi)     -> {prop,Phi}.
neg(Phi)      -> {neg,Phi}.
disj(Phi,Psi) -> {disj,Phi,Psi}.
conj(Phi,Psi) -> {conj,Phi,Psi}.
x(Phi)        -> {x,Phi}.
f(Phi)        -> {f,Phi}.
g(Phi)        -> {g,Phi}.
r(Phi,Psi)    -> {r,Phi,Psi}.
u(Phi,Psi)    -> {u,Phi,Psi}.
w(Phi,Psi)    -> {w,Phi,Psi}.
fn(Phi, 0)    -> prop(Phi);
fn(Phi, N)    -> disj(Phi,x(fn(Phi, N-1))).

showLTL({prop,P})       -> base:show(P);
showLTL({neg,Phi})      -> "(neg "++showLTL(Phi)++")";
showLTL({disj,Phi,Psi}) -> "("++showLTL(Phi)++" or "++showLTL(Psi)++")";
showLTL({conj,Phi,Psi}) -> "("++showLTL(Phi)++" and "++showLTL(Psi)++")";
showLTL({x,Phi})        -> "(X"++showLTL(Phi)++")";
showLTL({f,Phi})        -> "(F"++showLTL(Phi)++")";
showLTL({g,Phi})        -> "(G"++showLTL(Phi)++")";
showLTL({r,Phi,Psi})    -> "("++showLTL(Phi)++"R"++showLTL(Psi)++")";
showLTL({u,Phi,Psi})    -> "("++showLTL(Phi)++"U"++showLTL(Psi)++")";
showLTL({w,Phi,Psi})    -> "("++showLTL(Phi)++"W"++showLTL(Psi)++")";
showLTL(Phi)            -> base:show(Phi).

normalize(true)           -> true;
normalize(false)          -> false;
normalize({prop,P})       -> prop(P);
normalize({conj,Phi,Psi}) -> conj(normalize(Phi),normalize(Psi));
normalize({disj,Phi,Psi}) -> disj(normalize(Phi),normalize(Psi));
normalize({x,Phi})        -> x(normalize(Phi));
normalize({f,Phi})        -> f(normalize(Phi));
normalize({g,Phi})        -> g(normalize(Phi));
normalize({r,Phi,Psi})    -> r(normalize(Phi),normalize(Psi));
normalize({u,Phi,Psi})    -> u(normalize(Phi),normalize(Psi));
normalize({w,Phi,Psi})    -> w(normalize(Phi),normalize(Psi));
normalize({neg,true})           -> false;
normalize({neg,false})          -> true;
normalize({neg,{prop,P}})       -> neg(prop(P));
normalize({neg,{neg,Phi}})      -> normalize(Phi);
normalize({neg,{conj,Phi,Psi}}) -> disj(normalize(neg(Phi)),
                                        normalize(neg(Psi)));
normalize({neg,{disj,Phi,Psi}}) -> conj(normalize(neg(Phi)),
                                        normalize(neg(Psi)));
normalize({neg,{x,Phi}})        -> x(normalize(neg(Phi)));
normalize({neg,{f,Phi}})        -> g(normalize(neg(Phi)));
normalize({neg,{g,Phi}})        -> f(normalize(neg(Phi)));
normalize({neg,{r,Phi,Psi}})    -> u(normalize(neg(Phi)),normalize(neg(Psi)));
normalize({neg,{u,Phi,Psi}})    -> r(normalize(neg(Phi)),normalize(neg(Psi)));
normalize({neg,{w,Phi,Psi}})    -> normalize(neg(disj(u(Phi,Psi),g(Phi)))).


check(true,_Props)  -> true;
check(false,_Props) -> false;
check({prop,P},Props)       -> lists:member(P,Props);
check({neg,{prop,P}},Props) -> not(check(prop(P),Props));
check({conj,Phi,Psi},Props) ->
  case check(Phi,Props) of
    true  -> check(Psi,Props);
    false -> false;
    Phi1  -> case check(Psi,Props) of
               false -> false;
               true  -> Phi1;
               Psi1  -> conj(Phi1,Psi1)
             end
  end;
check({disj,Phi,Psi},Props) ->
  case check(Phi,Props) of
    true  -> true;
    false -> check(Psi,Props);
    Phi1  -> case check(Psi,Props) of
               true  -> true;
               false -> Phi1;
               Psi1  -> disj(Phi1,Psi1)
             end
  end;
check({x,Phi},_Props) -> x(Phi);
check({f,Phi}, Props) -> check(disj(Phi,x(f(Phi))),Props);
check({g,Phi}, Props) -> check(conj(Phi,x(g(Phi))),Props);
check({r,Phi,Psi}, Props) -> check(conj(Psi,disj(Phi,x(r(Phi,Psi)))), Props);
check({u,Phi,Psi}, Props) -> check(normalize(neg(r(neg(Phi),neg(Psi)))), Props);
check({w,Phi,Psi}, Props) -> check(disj(u(Phi,Psi),g(Phi)), Props);
check(Phi,_Props) -> base:putStrLn("Unexpected formula in check: "++
                                   showLTL(Phi)).




step({x,Phi}) -> Phi;
step({conj,Phi,Psi}) -> conj(step(Phi),step(Psi));
step({disj,Phi,Psi}) -> disj(step(Phi),step(Psi));
step(Phi) -> base:putStrLn("Unexpected formula in step: "++
                           showLTL(Phi)).

start() -> LTL = spawn(fun() -> ltl([],[],[],[],[]) end),
           register(ltl,LTL).

ltl(Phis,Asserts,Counts,Props,PropsHistory) ->
  receive
    {assert,Phi} -> case check(normalize(Phi),Props) of
                      true  -> ltl(Phis,Asserts,Counts,Props,PropsHistory);
                      false -> falsified(Phi,0,PropsHistory), 
                               ltl(Phis,Asserts,Counts,Props,PropsHistory);
                      Phi1  -> ltl([Phi1|Phis],[Phi|Asserts],[1|Counts],Props,PropsHistory)
                    end;
    {newProp,P} ->
      case lists:member(P,Props) of
        true -> ltl(Phis,Asserts,Counts,Props,PropsHistory);
        false ->
          NewProps = [P|Props],
          Phis1 = lists:map(fun(Phi) -> check(step(Phi),NewProps) end, Phis),
          {Phis2,Asserts2,Counts2} = analyze(Phis1,Asserts,Counts,PropsHistory),
          ltl(Phis2,Asserts2,Counts2,NewProps,[NewProps|PropsHistory])
      end;
    {releaseProp,P} ->
      NewProps = lists:delete(P,Props),
      Phis1 = lists:map(fun(Phi) -> check(step(Phi),NewProps) end, Phis),
      {Phis2,Asserts2,Counts2} = analyze(Phis1,Asserts,Counts,PropsHistory),
      ltl(Phis2,Asserts2,Counts2,NewProps,[NewProps|PropsHistory]);
    status ->
      base:printLn("Unevaluated Assertions:"),
      lists:zipwith3(fun(Phi,Assert,Count) ->
                      base:printLn(showLTL(Assert) ++ " passed " ++ base:show(Count) ++ " times."),
                      base:printLn("  "++showLTL(Phi)) end,
                    Phis,Asserts,Counts),
      ltl(Phis,Asserts,Counts,Props,PropsHistory)
  end.

assert(Phi)    -> ltl!{assert,Phi}.
newProp(P)     -> ltl!{newProp,P}.
releaseProp(P) -> ltl!{releaseProp,P}.
status()       -> ltl!status.  

falsified(Phi,C,PropsHistory) -> 
  base:printLn("Assertion violated: "++showLTL(Phi)++"\nafter "++base:show(C)++"checks."),
  base:printLn("The Path of Props leading to this error beginning with current Props: "),
  base:printLn(getPath(C,PropsHistory)),
  base:printLn(""),
  base:getLn("Continue: ").

getPath(_,[])           -> "";
getPath(0,[Prop|_]) -> base:show(Prop);
getPath(N,[Prop|Props]) -> base:show(Prop) ++ "\n" ++ getPath(N-1, Props). 

analyze([],[],[], _) -> {[],[],[]};
analyze([true|Phis], [_|Asserts], [_|Counts], PropsHistory) -> 
    analyze(Phis,Asserts,Counts, PropsHistory);
analyze([false|Phis],[A|Asserts], [C|Counts], PropsHistory) -> 
    falsified(A,C,PropsHistory), 
    analyze(Phis,Asserts,Counts, PropsHistory);
analyze([Phi|Phis],  [A|Asserts], [C|Counts], PropsHistory) ->
    {Phis1,Asserts1,Counts1} = analyze(Phis,Asserts,Counts, PropsHistory),
    {[Phi|Phis1],[A|Asserts1],[C+1|Counts1]}.



