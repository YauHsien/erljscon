-module(lexical).
-include("include/types.hrl").
-export(
   [ lex/1,
     lexer/1,
     literal/1,
     satisfy/1,
     string/1,
     tok/2
   ]).

cons({X, Xs}) ->
    [X|Xs].

-spec lex([{ parser(pos(char()), [char()]),
             tag()
           }])
         ->
          parser(pos(char()), [pos(token())]).
lex(PaTaList) ->
    combinators:many(lists:foldr(fun({P,T},Others) -> combinators:alt(tok(P,T),Others) end,
                                 fun primitives:fail/1,
                                 PaTaList)).

-spec lexer(Inp :: pos(char())) -> parser(pos(char()),pos(token())).
lexer(Inp) ->
    P = lex([ { combinators:some(formats:any(fun literal/1," \t\n")), 'Junk'   },
              { string("where"),                                      'Symbol' },
              { fun word/1,                                           'Ident'  },
              { fun number/1,                                         'Number' },
              { formats:any(fun string/1, ["(",")","="]),             'Symbol' }
            ]),
    P(Inp).

-spec literal(Case::'case'()) -> parser(pos('case'()),'case'()).
literal(X) ->
    satisfy(fun(Y) -> X==Y end).

-spec number(Inp :: [char()]) -> [{[char()],[char()]}].
number(Inp) ->
    P = combinators:some(satisfy(fun applications:digit/1)),
    P(Inp).

-spec satisfy(Predicate :: fun(('case'())->boolean())) -> parser(pos('case'()),'case'()).
satisfy(Predicate) ->
    fun([]) ->
            primitives:fail([]);
       ([{A,{_r,_c}}|Xs]) ->
            case Predicate(A) of
                true ->
                    (primitives:succeed(A))(Xs);
                false ->
                    primitives:fail(Xs)
            end
    end.

-spec string(Case :: ['case'()]) -> parser(from(),['case'()]).
string([]) ->
    primitives:succeed([]);
string([X|Xs]) ->
    combinators:using(combinators:then(literal(X),?REC(string(Xs))), fun cons/1).

-spec tok(
        parser(pos(char()), [char()]),
        tag()
       ) ->
          parser(pos(char()), token()).
tok(P, T) ->
    fun([{_x,{R,C}}|_]=Inp) ->
            [ {{{T,Xs},{R,C}},Out} || {Xs,Out} <- P(Inp) ];
       ([]) ->
            []
    end.

-spec word(Inp :: [char()]) -> [{[char()],[char()]}].
word(Inp) ->
    P = combinators:some(satisfy(fun applications:letter/1)),
    P(Inp).
