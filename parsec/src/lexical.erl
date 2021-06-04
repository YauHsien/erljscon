-module(lexical).
-include("include/types.hrl").
-export(
   [ literal/1,
     satisfy/1,
     string/1,
     tok/2
   ]).

cons({X, Xs}) ->
    [X|Xs].

-spec literal(Case::'case'()) -> parser(pos('case'()),'case'()).
literal(X) ->
    satisfy(fun(Y) -> X==Y end).

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

-spec string(Case :: ['case'()]) -> parser(from(),['case'()]).
string([]) ->
    primitives:succeed([]);
string([X|Xs]) ->
    combinators:using(combinators:then(literal(X),?REC(string(Xs))), fun cons/1).

-spec tok(
        parser(pos(char()), [char()]),
        tag()
       ) ->
          parser(pos(char()), token()).
tok(P, T) ->
    fun([{_x,{R,C}}|_]=Inp) ->
            [ {{{T,Xs},{R,C}},Out} || {Xs,Out} <- P(Inp) ]
    end.
