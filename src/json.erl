-module(json).
-compile(export_all).
-include("../include/json.hrl").

json() ->
    parser:alt(fn_util:lazy(fun object/0, []),
	       parser:alt(fn_util:lazy(fun array/0, []),
			  fn_util:lazy(fun value/0, []))).
      

value() ->
    parser:alt(?MODULE:string(),
	       parser:alt(num(),
			  parser:alt(fn_util:lazy(fun object/0, []),
				     parser:alt(fn_util:lazy(fun array/0, []),
						parser:alt(true(),
							   parser:alt(false(),
								      null())))))).

object() ->
    Cons = fun({A, B}) -> [A|B] end,
    Append = fun({A, B}) -> lists:append(A, B) end,
    Ignore = parser:p(fun parser:succeed/2, [""]),
    Id = fun(X) -> X end,
    EncObject = fun(E) -> #object{ elements= E } end,
    parser:using(
      parser:xthen(parser:nibble(parser:literal(${)),
		   parser:using(
		     parser:thenx(
		       parser:alt(Ignore,
				  parser:using(
				    parser:nibble(
				      parser:then(key_value(),
						  parser:nibble(
						    parser:some(then_kv())))),
				    Cons)),
		       parser:nibble(parser:literal($}))
		      ),
		     Id)),
      EncObject).
	       

array() ->
    Ignore = parser:p(fun parser:succeed/2, [""]),
    Unpack = fun([X]) -> X; (X) -> X end,
    parser:using(
      parser:xthen(parser:nibble(parser:literal($[)),
		   parser:thenx(
		     parser:alt(parser:using(parser:then(value(),
							 parser:some(then_value())),
					     fun cons/1),
				Ignore),
		     parser:nibble(parser:literal($])))),
		   Unpack).

then_value() ->
    parser:xthen(parser:nibble(parser:literal($,)),
		 value()).

key_value() ->
    parser:then(?MODULE:string(),
		parser:xthen(parser:nibble(parser:literal($:)),
			     parser:nibble(value()))).

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
    Cons = fun({X, Y}) -> [X|Y] end,
    Snoc = fun({X, Y}) -> lists:append(X, [Y]) end,
    Quote = parser:literal($"),
    Ignore = parser:p(fun parser:succeed/2, ""),
    parser:using(
      parser:then(parser:nibble(Quote),
		  parser:using(
		    parser:then(
		      parser:alt(Ignore,
				 parser:some(parser:alt(parser:acceptable_char(), escape_sequence()))),
		      Quote),
		    Snoc)),
      Cons).

integer() ->
    C2Str = fun(C) -> [C] end,
    Cons = fun({A, B}) -> [A|B] end,
    Ignore = parser:p(fun parser:succeed/2, ""),
    parser:alt(parser:using(parser:then(parser:digit1_9(),
					parser:alt(parser:some(parser:digit()), Ignore)),
			    Cons),
	       parser:using(parser:literal($0), C2Str)).

num() ->
    C2Str = fun(C) -> [C] end,
    Cons = fun({A, B}) -> [A|B] end,
    Append = fun({A, B}) -> lists:append(A, B) end,
    Ignore = parser:p(fun parser:succeed/2, ""),

    Sign = parser:nibble(parser:alt(Ignore,
				    parser:using(parser:literal($-), C2Str))),

    Decimal =
	parser:alt(parser:using(parser:then(parser:literal($.),
					    parser:some(parser:digit())),
				Cons),
		   Ignore),
    Exp =
	parser:alt(parser:using(parser:then(parser:alt(parser:literal($e), parser:literal($E)),
					    integer()),
				Cons),
		   Ignore),

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
