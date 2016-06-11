-module(xthen_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

xthen_test() ->
    P = parser:xthen(parser:string("hello"), parser:literal($,)),
    Input = "hello,world",
    V = P(Input),
    E = [#parsing{ parsed= $,, rest= "world" }],
    ?assertEqual(E, V).

