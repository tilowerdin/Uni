-module (tm2).
-behaviour (interfaceTM).
-export([new_tm/0, move/3, set_input/2]).

new_tm() -> spawn(fun() -> tm(0) end).

tm(Pos) ->
    receive
        {move, right, Write_symb, PID} -> self() ! {Pos, Write_symb},
                                          Symbol = get_symbol(Pos+1),
                                          PID!{moved,Symbol},
                                          tm(Pos+1);
        {move, left, Write_symb, PID}  -> self() ! {Pos, Write_symb},
                                          Symbol = get_symbol(Pos-1),
                                          PID!{moved,Symbol},
                                          tm(Pos-1);
        {move, no, Write_symb, PID}    -> PID!{moved,Write_symb},
                                          tm(Pos);
        {set_input, Input, PID}        -> empty_mailbox(),
                                          set_my_input(0, Input),
                                          PID ! {first, get_symbol(0)},
                                          tm(0)
    end.

get_symbol(Pos) ->
    receive
        {Pos, Symbol} -> Symbol
    after
        0 -> b
    end.

empty_mailbox() ->
    receive
        _M -> empty_mailbox()
    after
        0 -> ok
    end.

set_my_input(_, []) -> ok;
set_my_input(N, [X|XS]) -> self() ! {N,X},
                           set_my_input(N+1, XS).

move(TM, Direction, Write_symb) ->
    TM!{move, Direction, Write_symb, self()},
    receive 
        {moved, Read_symb} -> Read_symb
    end.

set_input(TM, Input) ->
    TM!{set_input, Input, self()},
    receive 
        {first, Symb} -> Symb
    end.

