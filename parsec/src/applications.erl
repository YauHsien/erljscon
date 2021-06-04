-module(applications).
-include("include/types.hrl").
-export(
  [ number/1,
    string/1,
    word/1
  ]).

-spec number(Inp :: [char()]) -> [{[char()],[char()]}].
number(Inp) ->
    P = combinators:some(primitives:satisfy(fun digit/1)),
    P(Inp).

digit(X) when $0 =< X andalso X =< $9 ->
    true;
digit(_) ->
    false.

-spec string(Case :: ['case'()]) -> parser(from(),['case'()]).
string([]) ->
    primitives:succeed([]);
string([X|Xs]) ->
    combinators:using(combinators:then(primitives:literal(X),?REC(string(Xs))), fun cons/1).

cons({X, Xs}) ->
    [X|Xs].

-spec word(Inp :: [char()]) -> [{[char()],[char()]}].
word(Inp) ->
    P = combinators:some(primitives:satisfy(fun letter/1)),
    P(Inp).

letter(X) when ($a =< X andalso X =< $z) orelse ($A =< X andalso X =< $Z) ->
    true;
letter(_) ->
    false.
