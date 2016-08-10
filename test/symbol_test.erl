-module(symbol_test).
-include_lib("eunit/include/eunit.hrl").
-include("../include/parsing.hrl").

symbol_test() ->
    P = parser:then(parser:symbol("hi"), parser:symbol("there")),
    Input = "  hi   there",
    V = P(Input),
    E = #parsing{ parsed= {"hi", "there"}, rest= "" },
    ?assertMatch([E|_], V).

