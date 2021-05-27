-module(arithmetic2).
-include("../include/types.hrl").
-export(
   [ expn/1,
     term/1,
     factor/1
   ]).
-define(alt, combinators:alt).
-define(using, combinators:using).
-define(then, combinators:then).
-define(thenx, combinators:thenx).
-define(xthen, combinators:xthen).
-define(number, fun applications:number/1).
-define(literal, primitives:literal).
-define(LPAR, $().
-define(RPAR, $)).

value(Xs) ->
    case string:to_integer(Xs) of
        {Num,_} -> Num
    end.

plus({X, Y}) -> X + Y.
minus({X, Y}) -> X - Y.
times({X, Y}) -> X * Y.
divide({X, Y}) -> X / Y.

expn(Inp) ->
    P = ?alt(
           ?alt(
              ?using(
                 ?then(?thenx(?REC(fun term/1),?literal($+)), ?REC(fun term/1)),
                 fun plus/1),
              ?using(
                 ?then(?thenx(?REC(fun term/1),?literal($-)), ?REC(fun term/1)),
                 fun minus/1)
             ),
           ?REC(fun term/1)),
    P(Inp).

term(Inp) ->
    P = ?alt(
           ?alt(
              ?using(
                 ?then(?thenx(?REC(fun factor/1),?literal($*)), ?REC(fun factor/1)),
                 fun times/1),
              ?using(
                 ?then(?thenx(?REC(fun factor/1),?literal($/)), ?REC(fun factor/1)),
                 fun divide/1)
             ),
           ?REC(fun factor/1)),
    P(Inp).

factor(Inp) ->
    P = ?alt(
           ?using(?number, fun value/1),
           ?thenx(?xthen(?literal(?LPAR),?REC(fun expn/1)), ?literal(?RPAR))
          ),
    P(Inp).
