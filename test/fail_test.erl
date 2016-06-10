-module(fail_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

fail_test() ->
    P = fn_util:curry(fun parser:fail/1),
    V = P("hello,world"),
    E = [],
    ?assertEqual(E, V).

p_test() ->
    P = parser:p(fun parser:fail/1),
    V = P("hello,world"),
    E = [],
    ?assertEqual(E, V).

