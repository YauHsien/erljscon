-module(formats).
-include("include/types.hrl").
-export(
   [ nibble/1,
     symbol/0
   ]).

-spec nibble(P::parser(char(),to())) -> parser(char(),to()).
nibble(P) ->
    combinators:xthen(white(), combinators:thenx(P,white())).

white() ->
    combinators:many(any(fun primitives:literal/1, " \t\n")).

-spec any(PF :: fun((V::'case'()) -> parser(from(),to())),
          List :: ['case'()]
         ) ->
          parser(from(), to()).
any(P, List) ->
    lists:foldl(
      fun(X,Acc)-> combinators:alt(?REC(P(X)),Acc) end,
      fun primitives:fail/1,
      List).

conjunct(G, F) ->
    fun(Inp) -> G(F(Inp)) end.

-spec symbol() -> fun(([char()]) -> parser(char(),[char()])).
symbol() ->
    conjunct(fun nibble/1, applications:string()).
