-module(arithmetic).
-include("../include/types.hrl").
-export(
   [ expn/0,
     term/0,
     factor/0
   ]).
-define(alt, combinators:alt).
-define(using, combinators:using).
-define(then, combinators:then).
-define(thenx, combinators:thenx).
-define(xthen, combinators:xthen).
-define(number, applications:number).
-define(literal, primitives:literal).
-define(LPAR, $().
-define(RPAR, $)).

value(Xs) ->
    case string:to_integer(Xs) of
        {Num,_} ->
            {num, Num}
    end.

plus({X, Y}) ->
    {add, X, Y}.

minus({X, Y}) ->
    {sub, X, Y}.

times({X, Y}) ->
    {mul, X, Y}.

divide({X, Y}) ->
    {'div', X, Y}.

expn() ->
    ?alt(
       ?alt(
          ?using(
             ?then(?thenx(?REC(term()),?literal($+)), ?REC(term())),
             fun plus/1),
          ?using(
             ?then(?thenx(?REC(term()),?literal($-)), ?REC(term())),
             fun minus/1)
         ),
       ?REC(term())).

term() ->
    ?alt(
       ?alt(
          ?using(
             ?then(?thenx(?REC(factor()),?literal($*)), ?REC(factor())),
             fun times/1),
          ?using(
             ?then(?thenx(?REC(factor()),?literal($/)), ?REC(factor())),
             fun divide/1)
         ),
       ?REC(factor())).

factor() ->
    ?alt(
       ?using(?number(), fun value/1),
       ?thenx(?xthen(?literal(?LPAR),?REC(expn())), ?literal(?RPAR))
      ).
