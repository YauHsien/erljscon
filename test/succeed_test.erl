-module(succeed_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

succeed_test() ->
    Parser = apply(parser:succeed(), [5]),
    Value = Parser("hello,world"),
    Expected = [#parsing{ parsed= 5, rest= "hello,world" }],
    ?assertEqual(Expected, Value).

