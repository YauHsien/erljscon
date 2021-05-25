-module(combinators).
-include("include/types.hrl").
-export(
   [ alt/2,
     many/1,
     return/2,
     some/1,
     then/2,
     thenx/2,
     using/2,
     xthen/2
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

-spec return(P::parser(from(),to()), V::'case'()) -> parser(from(),'case'()).
return(P, V) ->
    using(P, const(V)).

const(X) ->
    fun(_inp) -> X end.

-spec some(P :: parser(from(),to())) -> parser(from(),[to()]).
some(P) ->
    using(then(P,?REC(many(P))), fun cons/1).

-spec then(P1::parser(from(),to1()), P2::parser(from(),to2())) -> parser(from(),{to1(),to2()}).
then(P1, P2) ->
    fun(Inp) ->
            [{{V1,V2},Out2} || {V1,Out1}<-(unwrap(P1))(Inp), {V2,Out2}<-(unwrap(P2))(Out1) ]
    end.

-spec thenx(P1::parser(from(),to1()), P2::parser(from(),to2())) -> parser(from(),to1()).
thenx(P1, P2) ->
    using(then(P1,P2), fun fst/1).

fst({X,_}) ->
    X.

-spec using(P::parser(from(),'case'()), F::fun(('case'())->to())) -> parser(from(),to()).
using(P, F) ->
    fun(Inp) ->
            [{F(V),Out} || {V,Out}<-(unwrap(P))(Inp) ]
    end.

-spec xthen(P1::parser(from(),to1()), P2::parser(from(),to2())) -> parser(from(),to2()).
xthen(P1, P2) ->
    using(then(P1,P2), fun snd/1).

snd({_,X}) ->
    X.
