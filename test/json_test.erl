-module(json_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

unicode_test() ->
    P = json:unicode(),
    Inp = "\\u1234",
    V = P(Inp),
    E = [#parsing{ parsed= "\\u1234", rest= "" }],
    ?assertEqual(E, V).

