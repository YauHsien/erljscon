-module(formats_tests).
-include_lib("eunit/include/eunit.hrl").

nibble_test() ->
    P = formats:nibble(applications:string("hello")),
    Inp = "\n\thello ,world",
    ?assertMatch([{"hello",",world"},
                  {"hello"," ,world"}
                 ], P(Inp)),
    ok.

symbol_test() ->
    P = (formats:symbol())("hi"),
    Inp = "   hi   there",
    ?assertMatch([{"hi","there"},
                  {"hi"," there"},
                  {"hi","  there"},
                  {"hi","   there"}
                 ], P(Inp)),
    ok.
