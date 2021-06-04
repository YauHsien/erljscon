-module(offside).
-include("include/types.hrl").
-export(
   [ line_info/1,
     offside/1,
     onside/2,
     prelex/1,
     satisfy/1
   ]).

-spec line_info([char()]) -> [{char(),{integer(),integer()}}].
line_info(List) ->
    line_info(List, []).

line_info([], Acc) ->
    lists:reverse(Acc);
line_info([X|Rest], []) ->
    line_info(Rest, [{X,{1,1}}]);
line_info([X|Rest], [{$\n,{R,_c}}|_]=Acc) ->
    line_info(Rest, [{X,{R+1,1}}|Acc]);
line_info([X|Rest], [{_,{R,C}}|_]=Acc) ->
    line_info(Rest, [{X,{R,C+1}}|Acc]).

-spec offside(Parser :: parser(pos(from()),to())) -> parser(pos(from()),to()).
offside(Parser) ->
    fun([H|_]=Inp) ->
            InpOn = lists:takewhile(fun(Data) -> onside(H,Data) end, Inp),
            InpOff = lists:nthtail(erlang:length(InpOn), Inp),
            [ {V,InpOff} || {V,[]}<-Parser(InpOn) ]
    end.

onside({_,{R1,C1}}, {_,{R2,C2}}) ->
    R1 =< R2 andalso C1 =< C2.

-spec prelex(Inp :: [Term::any()]) -> fun(() -> [{ Term::any(), { Row::integer(), Column::integer() }}]).
prelex(Inp) ->
    F = pl({0, 0}),
    F(Inp).

pl({R, C}) ->
    fun([]) ->
            [];
       ([$\t|Xs]) ->
            [{$\t,{R,C}}|(pl({R,tab(C)}))(Xs)];
       ([$\n|Xs]) ->
            [{$\n,{R,C}}|(pl({R+1,0}))(Xs)];
       ([X|Xs]) ->
            [{X,{R,C}}|(pl({R,C+1}))(Xs)]
    end.

tab(C) ->
    ((C div 8)+1)*8.

-spec satisfy(Predicate :: fun(('case'())->boolean())) -> parser(pos('case'()),'case'()).
satisfy(Predicate) ->
    fun([]) ->
            primitives:fail([]);
       ([{A,{_r,_c}}|Xs]) ->
            case Predicate(A) of
                true ->
                    (primitives:succeed(A))(Xs);
                false ->
                    primitives:fail(Xs)
            end
    end.
