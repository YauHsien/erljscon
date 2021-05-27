-module(primitives).
-include("include/types.hrl").
-export(
  [ fail/1,
    literal/1,
    satisfy/1,
    succeed/1
  ]).

-spec fail(Inp::[from()]) -> [].
fail(Inp) -> [].

-spec literal(Case::'case'()) -> parser('case'(),'case'()).
literal(X) ->
    satisfy(fun(Y) -> X==Y end).

-spec satisfy(Predicate::fun(('case'())->boolean())) -> parser(from(),to()).
satisfy(Predicate) ->
    fun([]) ->
            fail([]);
       ([X|Xs]) ->
            case Predicate(X) of
                true ->
                    (succeed(X))(Xs);
                false ->
                    fail(Xs)
            end
    end.

-spec succeed(Val::to()) -> parser(from(),to()).
succeed(Val) ->
    fun(Inp) -> [{Val,Inp}] end.
