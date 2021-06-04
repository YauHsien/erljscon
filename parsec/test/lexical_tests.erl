-module(lexical_tests).
-include_lib("eunit/include/eunit.hrl").

lexer_test() ->
    P = fun lexical:lexer/1,
    Inp = "f x y",
    ?assertMatch([{[{{'Ident',"f"},{0,0}},
                    {{'Junk'," "},{0,1}},
                    {{'Ident',"x"},{0,2}},
                    {{'Junk'," "},{0,3}},
                    {{'Ident',"y"},{0,4}}],
                   []},
                  {[{{'Ident',"f"},{0,0}},
                    {{'Junk'," "},{0,1}},
                    {{'Ident',"x"},{0,2}},
                    {{'Junk'," "},{0,3}}],
                   [{121,{0,4}}]},
                  {[{{'Ident',"f"},{0,0}},
                    {{'Junk'," "},{0,1}},
                    {{'Ident',"x"},{0,2}}],
                   [{32,{0,3}},{121,{0,4}}]},
                  {[{{'Ident',"f"},{0,0}},{{'Junk'," "},{0,1}}],
                   [{120,{0,2}},{32,{0,3}},{121,{0,4}}]},
                  {[{{'Ident',"f"},{0,0}}],
                   [{32,{0,1}},{120,{0,2}},{32,{0,3}},{121,{0,4}}]},
                  {[],
                   [{102,{0,0}},
                    {32,{0,1}},
                    {120,{0,2}},
                    {32,{0,3}},
                    {121,{0,4}}]}],
                 P(offside:prelex(Inp))
                ),
    ok.

literal_test() ->
    P = lexical:literal($h),
    Inp = offside:prelex("hello,\nworld"),
    ?assertMatch([{$h,[{$e,{0,1}},{$l,{0,2}},{$l,{0,3}},{$o,{0,4}},{$,,{0,5}},{$\n,{0,6}},
                       {$w,{1,0}},{$o,{1,1}},{$r,{1,2}},{$l,{1,3}},{$d,{1,4}}]}],
                 P(Inp)
                ),
    ok.

satisfy_test() ->
    P = lexical:satisfy(fun(C)-> C==$h end),
    Inp = offside:prelex("hello,\nworld"),
    ?assertMatch([{$h,[{$e,{0,1}},{$l,{0,2}},{$l,{0,3}},{$o,{0,4}},{$,,{0,5}},{$\n,{0,6}},
                       {$w,{1,0}},{$o,{1,1}},{$r,{1,2}},{$l,{1,3}},{$d,{1,4}}]}],
                 P(Inp)
                ),
    ok.

tok_test() ->
    P = lexical:tok(lexical:string("where"), 'Symbol'),
    Inp = offside:prelex("where"),
    ?assertMatch([{{{'Symbol',"where"},{0,0}},[]}|_], P(Inp)),
    ok.
