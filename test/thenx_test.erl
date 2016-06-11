-module(thenx_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

thenx_test() ->
    P = parser:thenx(parser:string("hello"), parser:literal($,)),
    Input = "hello,world",
    V = P(Input),
    E = [#parsing{ parsed= "hello", rest= "world" }],
    ?assertEqual(E, V).

