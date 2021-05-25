-module(applications).
-include("include/types.hrl").
-export(
  [ number/0,
    string/0,
    word/0
  ]).

-spec number() -> parser(char(),[char()]).
number() ->
    combinators:some(primitives:satisfy(fun digit/1)).

digit(X) when $0 =< X andalso X =< $9 ->
    true;
digit(_) ->
    false.

-spec string() -> fun((['case'()]) -> parser(from(),['case'()])).
string() ->
    fun([]) ->
            primitives:succeed([]);
       ([X|Xs]) ->
            combinators:using(combinators:then(primitives:literal(X),?REC((string())(Xs))), fun cons/1)
    end.

cons({X, Xs}) ->
    [X|Xs].

-spec word() -> parser(char(),[char()]).
word() ->
    combinators:some(primitives:satisfy(fun letter/1)).

letter(X) when ($a =< X andalso X =< $z) orelse ($A =< X andalso X =< $Z) ->
    true;
letter(_) ->
    false.
