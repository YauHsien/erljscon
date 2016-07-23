-module(json).
-compile(export_all).
-include("../include/json.hrl").

value() ->
    parser:alt(?MODULE:string(),
	       parser:alt(num(),
			  parser:alt(fn_util:lazy(fun object/0, []),
				     parser:alt(fn_util:lazy(fun array/0, []),
						parser:alt(true(),
							   parser:alt(false(),
								      null())))))).

object() ->
    Ignore = parser:p(fun parser:succeed/2, [""]),
    parser:then(parser:literal(${),
		parser:then(parser:alt(parser:then(key_value(),
						   parser:alt(parser:many(then_kv()), Ignore)),
				       Ignore),
			    parser:literal($}))).

array() ->
    Ignore = parser:p(fun parser:succeed/2, [""]),
    parser:xthen(parser:literal($[),
		 parser:thenx(
		   parser:using(parser:then(value(),
					    parser:alt(parser:some(then_value()), Ignore)),
				fun cons/1),
		   parser:literal($]))).

then_value() ->
    parser:xthen(parser:literal($,), value()).

key_value() ->
    parser:then(?MODULE:string(),
		parser:xthen(parser:literal($:), value())).

then_kv() ->
    parser:xthen(parser:literal($,), key_value()).
	      

escape_sequence() ->
    lists:foldr(fun(Ch, R) -> parser:alt(escape(Ch), R) end,
		unicode(),
		[$", $\\, $\/, $b, $f, $n, $r, $t]).
			 

escape($") -> escape1($");
escape($\\) -> escape1($\\); 
escape($\/) -> escape1($\/);
escape($b) -> escape1($b);
escape($f) -> escape1($f); 
escape($n) -> escape1($n);
escape($r) -> escape1($r);
escape($t) -> escape1($t).

escape1(C) ->
    parser:using(
      parser:then(parser:literal($\\), parser:literal(C)),
      fun({A, B}) -> [A, B] end
     ).

hxdigit() ->
    lists:foldr(fun(Ch, R) -> parser:alt(parser:literal(Ch), R) end,
		parser:literal($F),
		[$0, $1, $2, $3, $4, $5, $6, $7, $8, $9,
		 $a, $b, $c, $d, $e, $f,
		 $A, $B, $C, $D, $E]).

hxdigits(0) ->
    parser:p(fun parser:succeed/2, "");
hxdigits(N) when N > 0 ->
    parser:using(parser:then(hxdigit(), hxdigits(N-1)),
		 fun({A,B}) -> [A|B] end).

unicode() ->
    parser:using(
      parser:then(parser:literal($\\),
		  parser:using(
		    parser:then(parser:alt(parser:literal($u),
					   parser:literal($U)),
				hxdigits(4)),
		    fun({A,B}) -> [A|B] end)
		 ),
      fun({A,B}) -> [A|B] end
     ).

string() ->
    Append = fun({A, B}) -> lists:append(A, B) end,
    Quote = parser:using(
	      parser:literal($"),
	      fun(Ch) -> [Ch] end
	     ),
    Ignore = parser:p(fun parser:succeed/2, ""),
    parser:using(
      parser:then(
	Quote,
	parser:using(
	  parser:then(
	    parser:alt(parser:some(parser:alt(parser:acceptable_char(), escape_sequence())),
		       Ignore),
	    Quote
	   ),
	  Append
	 )
       ),
      Append
     ).

integer() ->
    C2Str = fun(C) -> [C] end,
    Cons = fun({A, B}) -> [A|B] end,
    Ignore = parser:p(fun parser:succeed/2, ""),
    parser:alt(parser:using(parser:literal($0), C2Str),
	       parser:using(parser:then(parser:digit1_9(),
					parser:alt(parser:some(parser:digit()), Ignore)),
			    Cons)).

num() ->
    C2Str = fun(C) -> [C] end,
    Cons = fun({A, B}) -> [A|B] end,
    Append = fun({A, B}) -> lists:append(A, B) end,
    Ignore = parser:p(fun parser:succeed/2, ""),
    Sign =
	parser:alt(Ignore,
		   parser:using(parser:literal($-), C2Str)),
    Decimal =
	parser:alt(Ignore,
		   parser:using(parser:then(parser:literal($.),
					    parser:some(parser:digit())),
				Cons)),
    Exp =
	parser:alt(Ignore,
		   parser:using(parser:then(parser:alt(parser:literal($e), parser:literal($E)),
					    integer()),
				Cons)),
    parser:using(parser:then(Sign,
			     parser:using(parser:then(integer(),
						      parser:using(parser:then(Decimal, Exp), Append)),
					  Append)),
		 Append).

true() ->
    parser:nibble(parser:string("true")).

false() ->
    parser:nibble(parser:string("false")).

null() ->
    parser:nibble(parser:string("null")).


%% -------------------------------------
%% Internal
%% -------------------------------------

foldr(_F, Z, []) ->
    Z;
foldr(F, Z, [H|T]) ->
    F(H, foldr(F, Z, T)).

cons({A, B}) ->
    [A|B].

append({A, B}) ->
    lists:append(A, B).
