-module(linda).
-export([new_space/0,in/2,rd/2,out/2,in/3,out/3,rd/3]).

new_space() -> spawn(fun() -> linda([],[]) end).

linda(TS,Requests) ->
  receive
    {out,T,TimeOut}  -> Tuple = case TimeOut of
                                  -1 -> {T,0};
                                  _  -> {T, timeNow() + TimeOut}
                                end,
                        case find_matching_requests(Tuple,Requests) of
                          {true,New_Requests} -> linda([Tuple|TS], New_Requests);
                          {false,New_Requests} -> linda(TS,New_Requests)
                        end;
    {in,F,P,TimeOut} -> case find_matching_tuple(TS,F) of
                          nothing -> case TimeOut of
                                       -1 -> linda(TS,[{in,F,P,0}|Requests]);
                                       _  -> linda(TS,[{in,F,P,timeNow() + TimeOut}|Requests])
                                     end;
                          {just,{Res,NTS}} -> P!{found,Res},
                                              linda(NTS,Requests)
                        end;
    {rd,F,P,TimeOut} -> case find_matching_tuple(TS,F) of
                          nothing -> case TimeOut of
                                       -1 -> linda(TS,[{rd,F,P,0}|Requests]);
                                       _  -> linda(TS,[{in,F,P,timeNow() + TimeOut}|Requests])
                                     end;
                          {just,{Res,_}} -> P!{found,Res},
                                            linda(TS,Requests)
                        end;
    {getPID, P} -> P ! {server, self()},
      linda(TS,Requests)
  end.

% Has stack semantics, should be changed, exercise.
find_matching_tuple([],_) -> nothing;
find_matching_tuple([T|Ts],F) ->
  {Content, TimeStamp} = T,
  case (TimeStamp >= timeNow()) or (TimeStamp == 0) of
    false -> find_matching_tuple(Ts, F);
    true  -> case catch F(Content) of
               {'EXIT',_} -> case find_matching_tuple(Ts,F) of
                               nothing -> nothing;
                               {just,{FoundT,NTs}} -> {just,{FoundT,[T|NTs]}}
                             end;
               Res        -> {just,{Res,Ts}}
             end
  end.


% answer says whether or not to add the T to TS
% not if a request was found
find_matching_requests(_,[]) -> {true,[]};
find_matching_requests(T,[{Kind,F,P,TimeStamp}|Requests]) ->
  case (TimeStamp >= timeNow()) or (TimeStamp == 0) of
    false -> find_matching_requests(T, Requests);
    true  -> case find_matching_requests(T,Requests) of
               {false,New_Requests} -> {false,[{Kind,F,P,TimeStamp}|New_Requests]};
               {true,New_Requests}  ->
                 {Content,_Time} = T,
                 case catch F(Content) of
                   {'EXIT',_} -> {true,[{Kind,F,P,TimeStamp}|New_Requests]};
                   Res        -> P!{found,Res},
                     case Kind of
                       in -> {false,New_Requests};
                       rd -> {true ,New_Requests}
                     end
                 end
             end
  end.


%timeNow() ->
%  {MegaSeconds, Seconds, MicroSeconds} = erlang:now(),
%  (MegaSeconds*1000000 + Seconds)*1000000 + MicroSeconds.
timeNow() ->
  os:system_time().

out(TS,T) -> TS!{out,T,-1}.

in(TS,P) -> TS!{in,P,self(),-1},
            receive
              {found,Res} -> Res
            end. 

rd(TS,P) -> TS!{rd,P,self(),-1},
            receive
              {found,Res} -> Res
            end. 

% TimeOut is MicroSecs, < 0 is infinity
out(TS,T,TimeOut) ->
  case TimeOut =< -1 of
    true  -> TS!{out,T,-1};
    false -> TS!{out,T,TimeOut}
  end.

in(TS,P,TimeOut) ->
  case TimeOut =< -1 of
    true -> TS!{in,P,self(),-1},
      receive
        {found,Res} -> Res
      end;
    false -> TS!{in,P,self(),TimeOut},
      receive
        {found,Res} -> Res
      after
        (TimeOut div 1000) + 1 -> nothing
      end
  end.

rd(TS,P,TimeOut) ->
  case TimeOut =< -1 of
    true  -> TS!{rd,P,self(),-1},
      receive
        {found,Res} -> Res
      end;
    false -> TS!{rd,P,self(),TimeOut},
      receive
        {found,Res} -> Res
      after
        (TimeOut div 1000) + 1 -> nothing
      end
  end.











