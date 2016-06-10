-module(parser).
-compile(export_all).
-export([p/2, p/1, succeed/2, fail/1, satisfy/2, literal/1]).
-include("../include/parsing.hrl").
-define(DELAY(E), fun()-> E end).
-define(FORCE(F), F()).

%% Walking through Graham Hutton's paper: Higher-Order Functions for Parsing.

%% Parser constructors
%% -------------------

-spec p(function(), any()) -> parser().
p(F, A) ->
    (fn_util:curry(F))(A).

-spec p(function()) -> parser().
p(F) ->
    fn_util:curry(F).

%% Primitive parsers
%% -----------------

-spec succeed(any(), input()) -> [parsing()].
succeed(V, Inp) -> [#parsing{ parsed= V, rest= Inp }].

-spec fail(input()) -> [parsing()].
fail(_Inp) -> [].

-spec satisfy(predicate(), input()) -> [parsing()].
satisfy(_P, []) ->
    fail([]);
satisfy(P, [X|Xs]) ->
    case apply(P, [X]) of
	true ->
	    succeed(X, Xs);
	false ->
	    fail(Xs)
    end.

-spec literal(any()) -> parser().
literal(A) ->
    parser:p(fun parser:satisfy/2, fun(X) -> X == A end).

%% Combinators
%% -----------

-spec alt(parser(), parser()) -> parser().
alt(P1, P2) ->
    fun(Inp) ->
	    apply(P1, [Inp]) ++ apply(P2, [Inp])
    end.

-spec then(parser(), parser()) -> parser().
then(P1, P2) ->
    fun(Inp) ->
	    [ #parsing{ parsed= {V1, V2}, rest= Out2 }
	      || #parsing{ parsed= V1, rest= Out1 } <- apply(P1, [Inp]),
		 #parsing{ parsed= V2, rest= Out2 } <- apply(P2, [Out1]) ]
    end.

using(P, F) ->
    fun(Inp) ->
	    [ {apply(F, [V]), Out} || {V, Out} <- apply(P, [Inp]) ]
    end.

many(P) ->
    fun(Inp) ->
	    apply(alt(using(then(P, apply(fun many/1, [P]))
			    , fun({X,Xs}) -> [X|Xs] end)
		      , p(fun parser:succeed/2, []))
		  , [Inp])
    end.

some(P) ->
    fun(Inp) ->
	    apply(using(then(P, apply(fun many/1, [P]))
			, fun({X,Xs}) -> [X|Xs] end)
		  , [Inp])
    end.

%%=================================
number_() ->
    parser:p(fun parser:satisfy/2, fun(C) -> (C >= $0) and (C =< $9) end).

word() ->
    fun(Inp) ->
	    apply(some(parser:p(
			 fun parser:satisfy/2,
			 fun(C) -> ((C >= $a) and (C =< $z))
				       or ((C >= $A) and (C =< $Z))
			 end
			))
		      , [Inp])
    end.

string(Str) ->
    fun(Inp) ->
	    case Str of
		[] ->
		    apply(p(fun parser:succeed/2, []), [Inp]);
		[X|Xs] ->
		    apply(using(then(literal(X), string(Xs))
				, fun({Y,Ys}) -> [Y|Ys] end
				)
			  , [Inp])
	    end
    end.
%%==================================

xthen(P1, P2) ->
    using(then(P1, P2), fun({_x, Y}) -> Y end).

thenx(P1, P2) ->
    using(then(P1, P2), fun({X, _y}) -> X end).

return(P, V) ->
    using(P, apply(fun(X) -> fun(_y) -> X end end, [V])).

%%===================================
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
	      alt(using(number_(), fun(X) -> X end)
		  , xthen(literal($(), thenx(expn(), literal($))))
		 )
	      , [Inp])
    end.
%%====================================

non_control_char() ->
    fun(Inp) ->
	    apply(
	      parser:p(
		fun parser:satisfy/2,
		fun(C) -> (C =/= $") and (C =/= $\\) and (C > $\x{1F}) end),
	    [Inp])
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
	      alt(non_control_char()
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
    parser:p(fun parser:satisfy/2, fun(C) -> (C >= $0) and (C =< $9) end).

digit1_9() ->
    parser:p(fun parser:satisfy/2, fun(C) -> (C >= $1) and (C =< $9) end).

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

