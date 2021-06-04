-module(offside_tests).
-include_lib("eunit/include/eunit.hrl").

line_info_test() ->
    Inp = "hello,\nworld",
    Exp = [{$h,{1,1}},{$e,{1,2}},{$l,{1,3}},{$l,{1,4}},{$o,{1,5}},{$,,{1,6}},{$\n,{1,7}},
           {$w,{2,1}},{$o,{2,2}},{$r,{2,3}},{$l,{2,4}},{$d,{2,5}}
          ],
    ?assertMatch(Exp, offside:line_info(Inp)),
    ok.

prelex_test() ->
    Inp = "hello,\nworld",
    Exp = [{$h,{0,0}},{$e,{0,1}},{$l,{0,2}},{$l,{0,3}},{$o,{0,4}},{$,,{0,5}},{$\n,{0,6}},
           {$w,{1,0}},{$o,{1,1}},{$r,{1,2}},{$l,{1,3}},{$d,{1,4}}
          ],
    ?assertMatch(Exp, offside:prelex(Inp)),
    ok.


satisfy_test() ->
    P = offside:satisfy(fun(C)-> C==$h end),
    Inp = offside:line_info("hello,\nworld"),
    ?assertMatch([{$h,[{$e,{1,2}},{$l,{1,3}},{$l,{1,4}},{$o,{1,5}},{$,,{1,6}},{$\n,{1,7}},
                       {$w,{2,1}},{$o,{2,2}},{$r,{2,3}},{$l,{2,4}},{$d,{2,5}}]}],
                 P(Inp)
                ),
    ok.
