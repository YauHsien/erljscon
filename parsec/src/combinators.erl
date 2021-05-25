-module(combinators).
-include("include/types.hrl").
-export(
   [ alt/2,
     many/1,
     then/2,
     using/2
   ]).
-type to1() :: any().
-type to2() :: any().

unwrap({rec, W}) ->
    W();
unwrap(P) ->
    P.

-spec alt(P1::parser(from(),to()), P2::parser(from(),to())) -> parser(from(),to()).
alt(P1, P2) ->
    fun(Inp) ->
            erlang:append((unwrap(P1))(Inp), (unwrap(P2))(Inp))
    end.

-spec many(P :: parser(from(),to())) -> parser(from(),[to()]).
many(P) ->
    alt(using(then(P,?REC(many(P))), fun cons/1),
        primitives:succeed([])).

cons({X, Xs}) ->
    [X|Xs].

-spec then(P1::parser(from(),to1()), P2::parser(from(),to2())) -> parser(from,{to1(),to2()}).
then(P1, P2) ->
    fun(Inp) ->
            [{{V1,V2},Out2} || {V1,Out1}<-(unwrap(P1))(Inp), {V2,Out2}<-(unwrap(P2))(Out1) ]
    end.

-spec using(P::parser(from(),'case'()), F::fun(('case'())->to())) -> parser(from(),to()).
using(P, F) ->
    fun(Inp) ->
            [{F(V),Out} || {V,Out}<-(unwrap(P))(Inp) ]
    end.
