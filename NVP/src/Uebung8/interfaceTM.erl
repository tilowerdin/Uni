-module (interfaceTM).

-callback new_tm() -> Result :: pid().

-callback move(TM, Direction, Write_symb) -> Read_symb when
    TM         :: pid(),
    Direction  :: atom(),
    Write_symb :: integer(),
    Read_symb  :: integer().

-callback set_input(TM::pid(), Input::string()) -> Symb::integer().