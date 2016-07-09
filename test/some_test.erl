-module(some_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

some_digits_test() ->
    P = parser:some(parser:digit()),
    Inp = "1",
    V = P(Inp),
    E = [#parsing{ parsed= "1", rest= "" }],
    ?assertEqual(E, V).
