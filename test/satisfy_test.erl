-module(satisfy_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

acceptable_char_test() ->
    P = parser:acceptable_char(),
    Inp = "\"",
    V = P(Inp),
    E = [],
    ?assertEqual(E, V).
