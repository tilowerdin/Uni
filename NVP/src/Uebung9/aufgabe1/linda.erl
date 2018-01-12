-module(linda).
-export([new_space/0,in/2,rd/2,out/2]).

new_space() -> spawn(fun() -> linda([],[]) end).

linda(TS,Requests) ->
  receive
    {out,T}  -> case find_matching_requests(T,Requests) of
                  {true,New_Requests} -> linda([T|TS],New_Requests); 
                  {false,New_Requests} -> linda(TS,New_Requests) 
                end;
    {in,F,P} -> case find_matching_tuple(TS,F) of
                  nothing -> linda(TS,[{in,F,P}|Requests]); 
                  {just,{Res,NTS}} -> P!{found,Res},
                                      linda(NTS,Requests)
                end;
    {rd,F,P} -> case find_matching_tuple(TS,F) of
                  nothing -> linda(TS,[{rd,F,P}|Requests]); 
                  {just,{Res,_}} -> P!{found,Res},
                                    linda(TS,Requests)
                end
  end.

% Has stack semantics, should be changed, exercise.
find_matching_tuple([],_) -> nothing;
find_matching_tuple([T|Ts],F) ->
  case catch F(T) of
    {'EXIT',_} -> case find_matching_tuple(Ts,F) of
                    nothing -> nothing;
                    {just,{FoundT,NTs}} -> {just,{FoundT,[T|NTs]}}
                  end;
    Res        -> {just,{Res,Ts}}
  end.

find_matching_requests(_,[]) -> {true,[]};
find_matching_requests(T,[{Kind,F,P}|Requests]) ->
  case find_matching_requests(T,Requests) of
    {false,New_Requests} -> {false,[{Kind,F,P}|New_Requests]};
    {true,New_Requests}  -> 
      case catch F(T) of
        {'EXIT',_} -> {true,[{Kind,F,P}|New_Requests]};
        Res        -> P!{found,Res},
                      case Kind of
                        in -> {false,New_Requests};
                        rd -> {true ,New_Requests}
                      end
      end
  end.


out(TS,T) -> TS!{out,T}.

in(TS,P) -> TS!{in,P,self()},
            receive
              {found,Res} -> Res
            end. 

rd(TS,P) -> TS!{rd,P,self()},
            receive
              {found,Res} -> Res
            end. 













