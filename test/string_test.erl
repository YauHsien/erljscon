-module(string_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

string_test() ->
    P = parser:string("hello,world"),
    Input = "hello,world",
    V = P(Input),
    E = [#parsing{ parsed= "hello,world", rest= "" }],
    ?assertEqual(E, V).

