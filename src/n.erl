-module(n).
-export([json/1]).
-define(IOEMP, [<<>>]).
-define(IOLST(X), [X]).
-define(IOLST2(A,B), lists:append([A,B])).
-define(IOLST3(A,B,C), lists:append([A,B,C])).
-define(IOLST5(A,B,C,D,E), lists:append([A,B,C,D,E])).
-define(OK, ok).
-define(BadValue, badval).
-define(BadObject, badobj).
-define(BadMembers, badmbrs).
-define(BadMember, badmemb).
-define(BadArray, badarr).
-define(BadElements, badelms).
-define(BadElement, badelm).
-define(BadCharacters, badchrs).
-define(BadCharacter, badchar).
-define(BadEscape, badesc).
-define(BadString, badstr).
-define(BadNumber, badnum).
-define(BadInt, badint).
-define(BadFrac, badfrac).
-define(BadOneNine, badonin).
-define(BadDigits, baddgts).
-define(BadDigit, baddgt).
-define(BadHex, badhex).
-define(BadJSON, badjson).

%% Tokenizer meeting json.org
-spec(json(binary())
      -> {Tag :: atom(),
          Output :: iolist()}).
json(Bin) ->
    {?OK, E, _Tail, _Point} = json(Bin, {1, 1}),
    {?OK, E}.

-spec(json(Input :: binary(), Point :: {integer(), integer()})
      -> {Tag :: ?OK | ?BadJSON,
          Output :: iolist(),
          Tail :: binary(),
          Point1 :: {integer(), integer()}}).
json(Bin, {_r, _c} = Point) ->
    case element(Bin, Point) of
        {?OK, E, Tail, Point1} ->
            case Tail of
                <<>> -> {?OK, E, Tail, Point1};
                _ -> {?BadJSON, E, Tail, Point1}
            end;
        {?BadElement, E, Tail, Point1} ->
            {?BadJSON, E, Tail, Point1}
    end.

value(<<Sign:32, Tail/binary>>) when <<Sign:32>> == <<"true">> orelse <<Sign:32>> == <<"null">> ->
    {?OK, ?IOLST(<<Sign:32>>), Tail};
value(<<"false", Tail/binary>>) ->
    {?OK, ?IOLST(<<"false">>), Tail};
value(Bin) ->
    case object(Bin) of
        {?OK, O, Tail} ->
	    {?OK, O, Tail};
	{?BadObject, _, _} ->
	    case array(Bin) of
	        {?OK, A, Tail1} ->
		    {?OK, A, Tail1};
		{?BadArray, _, _} ->
		    case string(Bin) of
		        {?OK, S, Tail2} ->
			    {?OK, S, Tail2};
			{?BadString, _, _} ->
			    case number(Bin) of
			        {?OK, N, Tail3} ->
				    {?OK, N, Tail3};
				{?BadNumber, _, _} ->
				    {?BadValue, ?IOLST(<<>>), Bin}
			    end
		    end
	    end
    end.

object(Bin) ->
    try
        <<${, Tail/binary>> = Bin,
	{?OK, Ms, Tail1} =
	    case members(Tail) of
	        {?OK, _, _} = R1 ->
		    R1;
		{?BadMembers, _, _} ->
		    t:ws(Tail)
	    end,
	<<$}, Tail2/binary>> = Tail1,
        {?OK, ?IOLST3(?IOLST(<<${>>), Ms, ?IOLST(<<$}>>)), Tail2}
    catch
        _:_ -> {?BadObject, ?IOEMP, Bin}
    end.

members(Bin) ->
    case member(Bin) of
        {?OK, M, Tail} ->
	    try
	        <<$,, Tail1/binary>> = Tail,
		{?OK, M1, Tail2} = member(Tail1),
		{?OK, ?IOLST3(M, ?IOLST(<<$,>>), M1), Tail2}
	    catch
	        _:_ -> {?OK, M, Tail}
	    end;
	{?BadMember, _, _} ->
	    {?BadMembers, ?IOEMP, Bin}
    end.

member(Bin) ->
    try
        {?OK, WS1, Tail} = t:ws(Bin),
	{?OK, S, Tail1} = string(Tail),
	{?OK, WS2, Tail2} = t:ws(Tail1),
	<<$:, Tail3/binary>> = Tail2,
	{?OK, E, Tail4} = element(Tail3),
	{?OK, ?IOLST5(WS1, S, WS2, ?IOLST(<<$:>>), E), Tail4}
    catch
        _:_ -> {?BadMember, ?IOEMP, Bin}
    end.

array(Bin) ->
    try
        <<$[, Tail/binary>> = Bin,
	{?OK, Es, Tail1} =
	    case elements(Tail) of
	        {?OK, _, _} = R1 ->
		    R1;
		{?BadElements, _, _} ->
		    t:ws(Tail)
	    end,
	<<$], Tail2/binary>> = Tail1,
	{?OK, ?IOLST3(?IOLST(<<$[>>), Es, ?IOLST(<<$]>>)), Tail2}
    catch
        _:_ -> {?BadArray, ?IOEMP, Bin}
    end.

elements(Bin) ->
    case element(Bin) of
        {?OK, E, Tail} ->
	    try
	        <<$,, Tail1/binary>> = Tail,
		{?OK, E1, Tail2} = element(Tail1),
		{?OK, ?IOLST3(E, ?IOLST(<<$,>>), E1), Tail2}
	    catch
	        _:_ -> {?OK, E, Tail}
	    end;
	{?BadElement, _, _} ->
	    {?BadElements, <<>>, Bin}
    end.

-spec(element(Input :: binary(), Point :: {integer(), integer()})
      -> {Tag :: ?OK | ?BadElement,
          Parsed :: iolist(),
          Tail :: binary(),
          Point1 :: {integer(), integer()}}).
element(Bin, {_r, _c} = Point) ->
    {?OK, WS1, Tail, Point1} = t:ws(Bin, Pointer),
    case value(Tail, Point1) of
        {?OK, V, Tail1, Point2} ->
            {?OK, WS2, Tail2, Point3} = t:ws(Tail1, Point2),
            {?OK, ?IOLST3(WS1, V, WS2), Tail2, Point3};
        {?BadValue, V, Tail1, Point2} ->
            {?BadElement, ?IOLST(V), Tail1, Point2}
    end.

string(Bin) ->
    try
        <<$", Tail/binary>> = Bin,
        {?OK, Cs, Tail1} = characters(Tail),
        <<$", Tail2/binary>> = Tail1,
	{?OK, ?IOLST3(?IOLST(<<$">>), Cs, ?IOLST(<<$">>)), Tail2}
    catch
        _:_ -> {?BadString, ?IOEMP, Bin}
    end.

characters(Bin) ->
    case character(Bin) of
        {?OK, C, Tail} ->
	    case characters(Tail) of
	        {?OK, Cs, Tail1} ->
		    {?OK, ?IOLST2(C, Cs), Tail1};
		{?BadCharacters, _, _} ->
		    {?OK, C, Tail}
            end;
	{?BadCharacter, _, _} ->
	    {?BadCharacters, ?IOEMP, Bin}
    end.

character(<<$\\, Tail>>) ->
    case escape(Tail) of
        {?OK, E, Tail1} ->
	    {?OK, ?IOLST2(?IOLST(<<$\\>>), E), Tail1};
	{?BadEscape, _, _} ->
	    {?BadCharacter, ?IOEMP, Tail}
    end;
character(<<C/integer, Tail>>) when 16#20 =< C andalso C =< 16#10ffff andalso C =/= $" andalso C =/= $\ ->
    {?OK, ?IOLST(<<C/integer>>), Tail};
character(Bin) ->
    {?BadCharacter, ?IOEMP, Bin}.

escape(<<$", Tail>>) -> {ok, ?IOLST(<<$">>), Tail};
escape(<<$\\, Tail>>) -> {ok, ?IOLST(<<$\\>>), Tail};
escape(<<$/, Tail>>) -> {ok, ?IOLST(<<$/>>), Tail};
escape(<<$b, Tail>>) -> {ok, ?IOLST(<<$b>>), Tail};
escape(<<$n, Tail>>) -> {ok, ?IOLST(<<$n>>), Tail};
escape(<<$r, Tail>>) -> {ok, ?IOLST(<<$r>>), Tail};
escape(<<$t, Tail>>) -> {ok, ?IOLST(<<$t>>), Tail};
escape(<<$u, Tail>>) ->
    try
        {?OK, H1, Tail1} = hex(Tail),
        {?OK, H2, Tail2} = hex(Tail1),
        {?OK, H3, Tail3} = hex(Tail2),
        {?OK, H4, Tail4} = hex(Tail3),
        {?OK, ?IOLST5(?IOLST(<<$u>>), H1, H2, H3, H4), Tail4}
    catch
        _:_ -> {?BadEscape, ?IOEMP, <<$u, Tail>>}
    end;
escape(Bin) ->
    {?BadEscape, ?IOEMP, Bin}.

hex(<<C/integer, Tail>>) when C == $a orelse C == $b orelse C == $c orelse C == $d orelse C == $e orelse C == $f orelse C == $A orelse C == $B orelse C == $C orelse C == $D orelse C == $E orelse C == $F ->
    {?OK, ?IOLST(<<C/integer>>), Tail};
hex(Bin) ->
    case digit(Bin) of
        {?BadDigit, D, Tail} -> {?BadHex, D, Tail};
        R1 -> R1
    end.

number(Bin) ->
    try
        {?OK, I, Tail} = int(Bin),
	{?OK, F, Tail1} = frac(Tail),
	{?OK, E, Tail2} = exp(Tail1),
        {?OK, ?IOLST3(I, F, E), Tail2}
    catch
        _:_ -> {?BadNumber, ?IOEMP, Bin}
    end.

int(<<$-, Tail>>) ->
    case t:onenine(Tail) of
        {?OK, OneNine, Tail1} ->
	    case digits(Tail1) of
	        {?OK, Ds, Tail2} ->
		    {?OK, ?IOLST3(?IOLST(<<$->>), OneNine, Ds), Tail2};
		{?BadDigits, _, _} ->
		    {?OK, ?IOLST2(?IOLST(<<$->>), OneNine), Tail1}
	    end;
	{?BadOneNine, _, _} ->
	    case digit(Tail) of
	        {?OK, D, Tail1} ->
		    {?OK, ?IOLST2(?IOLST(<<$->>), D), Tail1};
		{?BadDigit, _, _} ->
		    {?BadInt, ?IOLST(<<$->>), Tail}
	    end
    end;
int(Bin) ->
    case t:onenine(Bin) of
        {?OK, OneNine, Tail1} ->
	    case digits(Tail1) of
	        {?OK, Ds, Tail2} ->
		    {?OK, ?IOLST2(OneNine, Ds), Tail2};
		{?BadDigits, _, _} ->
		    {?OK, OneNine, Tail1}
	    end;
	{?BadOneNine, _, _} ->
	    case digit(Bin) of
	        {?OK, D, Tail1} ->
		    {?OK, D, Tail1};
		{?BadDigit, _, _} ->
		    {?BadInt, ?IOEMP, Bin}
	    end
    end.

digits(Bin) ->
    case digit(Bin) of
        {?OK, D, Tail} ->
	    case digits(Tail) of
	        {?OK, Ds, Tail1} ->
		    {?OK, ?IOLST2(D, Ds), Tail1};
		{?BadDigits, _, _} ->
		    {?OK, D, Tail}
	    end;
	{?BadDigit, _, _} ->
	    {?BadDigits, ?IOEMP, Bin}
    end.

digit(<<$0, Tail>>) ->
    {?OK, ?IOLST(<<$0>>), Tail};
digit(Bin) ->
    case t:onenine(Bin) of
        {?OK, OneNine, Tail} ->
	    {?OK, OneNine, Tail};
	{?BadOneNine, _, _} ->
	    {?BadDigit, ?IOEMP, Bin}
    end.

frac(<<$., Tail>>) ->
    case digits(Tail) of
        {?OK, D, Tail1} ->
	    {ok, ?IOLST2(?IOLST(<<$.>>), D), Tail1};
        {?BadDigits, _, _} ->
	    {?BadFrac, ?IOLST(<<$.>>), Tail}
    end;
frac(Bin) ->
    {?BadFrac, ?IOEMP, Bin}.

exp(Bin) ->
    try
        <<C/integer, Tail/binary>> = Bin,
        true = C == $E orelse C == $e,
        {?OK, S, Tail1} = t:sign(Tail),
        {?OK, D, Tail2} = digits(Tail1),
        {?OK, ?IOLST3(?IOLST(<<C/integer>>), S, D), Tail2}
    catch
        _:_ -> {?OK, ?IOEMP, Bin}
    end.
