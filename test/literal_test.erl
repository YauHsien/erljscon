-module(literal_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

p_test() ->
    P = parser:p(fun parser:literal/1, $3),
    Vt = P("345"),
    Et = [#parsing{ parsed= $3, rest= "45" }],
    Vf = P("hello,world"),
    Ef = [],
    ?assertEqual(Et, Vt),
    ?assertEqual(Ef, Vf).

