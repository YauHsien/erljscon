-module(parser).
-compile(export_all).
-export([p/2, p/1, succeed/2, fail/1, satisfy/2, literal/1]).
-include("../include/parsing.hrl").
-include("../include/lazy.hrl").
-define(DELAY(E), fun()-> E end).
-define(FORCE(F), F()).

%% Walking through Graham Hutton's paper: Higher-Order Functions for Parsing.

%% Parser constructors
%% -------------------

-spec p(function(), to_parse(A)) -> parser(A, any()).
p(F, A) ->
    (fn_util:curry(F))(A).

-spec p(function()) -> parser(any(), any());
       (#lazy{}) -> parser(any(), any()).
p(F) when is_function(F) ->
    fn_util:curry(F);
p(#lazy{ function= F, args= As }) ->
    apply(F, As).

%% Primitive parsers
%% -----------------

-spec succeed(to_parse(A), input(B)) -> [parsing(B, A)].
succeed(V, Inp) -> [#parsing{ parsed= V, rest= Inp }].

-spec fail(input(T)) -> [parsing(any(), T)].
fail(_Inp) -> [].

-spec satisfy(predicate(A), input(A)) -> [parsing(A, A)].
satisfy(_P, []) ->
    fail([]);
satisfy(P, [X|Xs]) ->
    case apply(P, [X]) of
	true ->
	    succeed(X, Xs);
	false ->
	    fail(Xs)
    end.

-spec literal(T) -> parser(T, any()).
literal(A) ->
    parser:p(fun parser:satisfy/2, fun(X) -> X == A end).

%% Combinators
%% -----------

-spec alt(parser(A, B), parser(A, B)) -> parser(A, B).
alt(P1, P2) ->
    fun(Inp) ->
	    fn_util:apply(P1, [Inp]) ++ fn_util:apply(P2, [Inp])
    end.

-spec then(parser(A, B), parser(A, B)) -> parser(A, B).
then(P1, P2) ->
    fun(Inp) ->
	    [ #parsing{ parsed= {V1, V2}, rest= Out2 }
	      || #parsing{ parsed= V1, rest= Out1 } <- fn_util:apply(P1, [Inp]),
		 #parsing{ parsed= V2, rest= Out2 } <- fn_util:apply(P2, [Out1]) ]
    end.

-spec using(parser(A, B), function(B, C)) -> parser(A, C).
using(P, F) ->
    fun(Inp) ->
	    [ Parsing#parsing{ parsed= F(Parsing#parsing.parsed) }
	      || #parsing{}= Parsing <- fn_util:apply(P, [Inp]) ]
    end.

-spec many(parser(A, B)) -> parser(A, [B]).
many(P) ->
    P1 = parser:then(P, fn_util:lazy(fun parser:many/1, [P])),
    P2 = parser:using(P1, fun cons/1),
    P3 = parser:p(fun parser:succeed/2, []),
    parser:alt(P2, P3).

-spec some(parser(A, B)) -> parser(A, [B]).
some(P) ->
    P1 = parser:then(P, fn_util:lazy(fun parser:many/1, [P])),
    parser:using(P1, fun cons/1).

-spec xthen(parser(A, _B), parser(A, C)) -> parser(A, C).
xthen(P1, P2) ->
    parser:using(parser:then(P1, P2), fun snd/1).

-spec thenx(parser(A, B), parser(A, _C)) -> parser(A, B).
thenx(P1, P2) ->
    parser:using(parser:then(P1, P2), fun fst/1).

-spec return(parser(A, _B), C) -> parser(A, C).
return(P, V) ->
    parser:using(P, (fn_util:curry(fun const/2))(V)).

-spec any(fun((A) -> parser(B, C)), [A]) -> parser(B, C).
any(Pg, Inp) ->
    lists:foldr(fun(X, P1) -> parser:alt(Pg(X), P1) end,
		parser:p(fun parser:fail/1),
		Inp).

-spec nibble(parser(char(), A)) -> parser(char(), A).
nibble(P) ->
    parser:xthen(white(), parser:thenx(P, white())).

%% Internal functions
%% ------------------

cons({X, Xs}) ->
    [X|Xs].

fst({X, _}) ->
    X.

snd({_, X}) ->
    X.

const(X, _) ->
    X.

%%=================================
is_digit(C) ->
    C >= $0 andalso C =< $9.

is_digit_1_9(C) ->
    C >= $1 andalso C =< $9.

is_letter(C) ->
    (C >= $a andalso C =< $z) orelse
	(C >= $A andalso C =< $Z).

-spec num() -> parser(char(), [char()]).
num() ->
    parser:some(parser:p(fun parser:satisfy/2, fun is_digit/1)).

-spec word() -> parser(char(), [char()]).
word() ->
    parser:some(parser:p(fun parser:satisfy/2, fun is_letter/1)).

-spec string([char()]) -> parser(char(), [char()]).
string([]) ->
    parser:p(fun succeed/2, []);
string([X|Xs]) ->
    P = parser:then(parser:literal(X), fn_util:lazy(fun parser:string/1, [Xs])),
    parser:using(P, fun cons/1).

white() ->
    parser:many(parser:any(fun parser:literal/1, " \t\r\n")).

-spec symbol([char()]) -> parser(char(), [char()]).
symbol(S) ->
    parser:nibble(parser:string(S)).

%% Examples
%% --------

expn() ->
    fun(Inp) ->
	    apply(
	      alt(using(then(term(), xthen(literal($+), term()))
			, fun({X, Y}) -> {X, "+", Y} end)
		  , alt(using(then(term(), xthen(literal($-), term()))
			      , fun({X, Y}) -> {X, "-", Y} end)
			, term())
		 )
	      , [Inp])
    end.

term() ->
    fun(Inp) ->
	    apply(
	      alt(using(then(factor(), xthen(literal($*), factor()))
			, fun({X, Y}) -> {X, "*", Y} end)
		  , alt(using(then(factor(), xthen(literal($/), factor()))
			      , fun({X, Y}) -> {X, "/", Y} end)
			, factor())
		 )
	      , [Inp])
    end.

factor() ->
    fun(Inp) ->
	    apply(
	      alt(using(num(), fun(X) -> X end)
		  , xthen(literal($(), thenx(expn(), literal($))))
		 )
	      , [Inp])
    end.
%%====================================

acceptable_char() ->
    fun(Inp) ->
	    apply(
	      parser:p(
		fun parser:satisfy/2,
		fun(C) ->
			(C =/= $")
			    andalso (C =/= $\\)
			    andalso (C > $\x{1F}) %% not control chars
		end
	       ),
	      [Inp]
	     )
    end.

double_quote() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($")), [Inp])
    end.

back_slash() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($\\)), [Inp])
    end.

slash() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($/)), [Inp])
    end.

bksp() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($b)), [Inp])
    end.

ff() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($f)), [Inp])
    end.

nl() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($n)), [Inp])
    end.

cr() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($r)), [Inp])
    end.

tab() ->
    fun(Inp) ->
	    apply(then(literal($\\), literal($t)), [Inp])
    end.

unicode() ->
    fun(Inp) ->
	    apply(
	      using(then(literal($\\)
		   , using(then(literal($u)
			  , using(then(hex_digit()
				 , using(then(hex_digit()
					, using(then(hex_digit(), hex_digit()
						    ), fun stdlib:cons/1)
				       ), fun stdlib:cons/1)
				 ), fun stdlib:cons/1)
		       ), fun stdlib:cons/1)
	       ), fun stdlib:cons/1)
	      , [Inp])
    end.

hex_digit() ->
    fun(Inp) ->
	    apply(parser:p(
		    fun parser:satisfy/2,
		    fun(C) -> ((C >= $0) and (C =< $9))
				  or ((C >= $a) and (C =< $f))
				  or ((C >= $A) and (C =< $F))
		    end)
		 , [Inp])
    end.

%%=====================================
char() ->
    fun(Inp) ->
	    apply(
	      alt(acceptable_char()
		  , alt(using(double_quote(), fun stdlib:cons/1)
			, alt(using(back_slash(), fun stdlib:cons/1)
			      , alt(using(slash(), fun stdlib:cons/1)
				    , alt(using(bksp(), fun stdlib:cons/1)
					  , alt(using(ff(), fun stdlib:cons/1)
						, alt(using(nl(), fun stdlib:cons/1)
						      , alt(using(cr(), fun stdlib:cons/1)
							    , alt(using(tab(), fun stdlib:cons/1)
								  , unicode()
								 )))))))))
	      , [Inp])
    end.

chars() ->
    fun(Inp) ->
	    apply(some(char()), [Inp])
    end.

string() ->
    fun(Inp) ->
	    apply(
	      alt(xthen(literal($"), thenx(p(fun parser:succeed/2, []), literal($")))
		  , xthen(literal($"), thenx(chars(), literal($")))
		 )
	      , [Inp])
    end.

%%=================================
e() ->
    alt(parser:p(fun parser:satisfy/2, fun(C) -> (C == $e) or (C == $E) end)
	, then(parser:p(fun parser:satisfy/2, fun(C) -> (C == $e) or (C == $E) end),
	       parser:p(fun parser:satisfy/2, fun(C) -> (C == $+) or (C == $-) end))).

digit() ->
    parser:p(fun parser:satisfy/2, fun is_digit/1).

digit1_9() ->
    parser:p(fun parser:satisfy/2, fun is_digit_1_9/1).

digits() ->
    some(digit()).

exp() ->
    using(then(e(), digits())
	  , fun(Tuple) ->
		    case Tuple of
			{$e, Num} ->
			    [$e|Num];
			{$E, Num} ->
			    [$e|Num];
			{{$e, Sign}, Num} ->
			    case Sign of
				$+ ->
				    [$e|Num];
				$- ->
				    [$e, $-|Num]
			    end;
			{{$E, Sign}, Num} ->
			    case Sign of
				$+ ->
				    [$e|Num];
				$- ->
				    [$e, $-|Num]
			    end
		    end
	    end
	 ).

frac() ->
    using(then(literal($.), digits()), fun stdlib:cons/1).

int() ->
    alt(using(then(digit(), p(fun parser:succeed/2, [])), fun stdlib:cons/1)
	, alt(using(then(digit1_9(), digits()), fun stdlib:cons/1)
	      , alt(using(then(literal($-), digit()), fun stdlib:cons/1)
		    , using(then(then(literal($-), digit1_9()), digits()), fun({{$-, D}, Num}) -> [$-, D|Num] end)
		    ))).

number() ->
    alt(int()
	, alt(using(then(int(), frac()), fun stdlib:append/1)
	      , alt(using(then(int(), exp()), fun stdlib:append/1)
		    , using(then(using(then(int(), frac()), fun stdlib:append/1), exp()), fun stdlib:append/1)
		    ))).

true() ->
    using(string("true"), fun(T) -> case T of "true" -> true; _ -> nil end end).

false() ->
    using(string("false"), fun(T) -> case T of "false" -> false; _ -> nil end end).

null() ->
    using(string("null"), fun(N) -> case N of "null" -> null; _ -> nil end end).

skip() ->
    xthen(
      some(
	alt(
	  literal($\x{20})
	  , alt(
	      nl()
	      , alt(
		  cr()
		  , tab())))
       )
      , p(fun parser:succeed/2, [])
     ).

