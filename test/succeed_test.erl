-module(succeed_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

succeed_test() ->
    S = fn_util:curry(fun parser:succeed/2),

    Parser = S(5),
    ?assert(is_function(Parser)),
    ?assertEqual({fn_util, '-curry/3-fun-0-', 1}, fn_util:parts(Parser)),

    V = Parser("hello,world"),
    E = [#parsing{ parsed= 5, rest= "hello,world" }],
    ?assertEqual(E, V).

p_test() ->
    P = parser:p(fun parser:succeed/2, 7),
    V = P("hello,world"),
    E = [#parsing{ parsed= 7, rest= "hello,world" }],
    ?assertEqual(E, V).
