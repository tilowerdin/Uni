-module (tm1).
-export ([new_tm/0,left/2,right/2]).

new_tm() -> spawn(fun() -> tm(b,[],[]) end).

tm(Symb, LBand, RBand) ->
    receive
        {setInput, Input} -> 
    end