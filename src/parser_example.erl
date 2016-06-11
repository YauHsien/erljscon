-module(parser_example).
-export([expn/0, eval/1]).
-include("../include/parsing.hrl").
-include("../include/example.hrl").

expn() ->
    Term = fn_util:lazy(fun term/0, []),
    P1 = parser:using(parser:then(Term, parser:xthen(parser:literal($+), Term)),
		      fun plus/1),
    P2 = parser:using(parser:then(Term, parser:xthen(parser:literal($-), Term)),
		      fun minus/1),
    parser:alt(parser:alt(P1, P2), Term).

term() ->
    Factor = fn_util:lazy(fun factor/0, []),
    P1 = parser:using(parser:then(Factor, parser:xthen(parser:literal($*), Factor)),
		      fun times/1),
    P2 = parser:using(parser:then(Factor, parser:xthen(parser:literal($/), Factor)),
		      fun divide/1),
    parser:alt(parser:alt(P1, P2), Factor).

factor() ->
    Expn = fn_util:lazy(fun parser_example:expn/0, []),
    P1 = parser:using(parser:num(), fun value/1),
    P2 = parser:xthen(parser:literal($(),
		      parser:thenx(Expn,
				   parser:literal($)))),
    parser:alt(P1, P2).

value(V) ->
    #num{ value= V }.

plus({A, B}) ->
    #plus{ left= A, right= B }.

minus({A, B}) ->
    #minus{ left= A, right= B }.

times({A, B}) ->
    #times{ left= A, right= B }.

divide({A, B}) ->
    #divide{ left= A, right= B }.

%% --------------

eval(Expn) ->
    eval1((expn())(Expn)).

eval1(ParsingList) when is_list(ParsingList) ->
    lists:map(fun(P) -> eval1(P) end, ParsingList);
eval1(#parsing{}= Expn) ->
    Expn#parsing{ parsed= eval1(Expn#parsing.parsed) };
eval1(#num{ value= V }) ->
    {Num, _} = string:to_integer(V),
    Num;
eval1(#plus{ left= L, right= R }) ->
    eval1(L) + eval1(R);
eval1(#minus{ left= L, right= R }) ->
    eval1(L) - eval1(R);
eval1(#times{ left= L, right= R }) ->
    eval1(L) * eval1(R);
eval1(#divide{ left= L, right= R }) ->
    eval1(L) / eval1(R).

