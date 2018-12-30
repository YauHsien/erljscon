-module(t).
-export([onenine/1, sign/1, ws/1]).
-define(IOEMP, [<<>>]).
-define(IOLST(X), [X]).
-define(OK, ok).
-define(BadOneNine, badonin).
-define(BadResult, badret).

onenine(<<$1, Tail>>) -> {?OK, ?IOLST(<<$1>>), Tail};
onenine(<<$2, Tail>>) -> {?OK, ?IOLST(<<$2>>), Tail};
onenine(<<$3, Tail>>) -> {?OK, ?IOLST(<<$3>>), Tail};
onenine(<<$4, Tail>>) -> {?OK, ?IOLST(<<$4>>), Tail};
onenine(<<$5, Tail>>) -> {?OK, ?IOLST(<<$5>>), Tail};
onenine(<<$6, Tail>>) -> {?OK, ?IOLST(<<$6>>), Tail};
onenine(<<$7, Tail>>) -> {?OK, ?IOLST(<<$7>>), Tail};
onenine(<<$8, Tail>>) -> {?OK, ?IOLST(<<$8>>), Tail};
onenine(<<$9, Tail>>) -> {?OK, ?IOLST(<<$9>>), Tail};
onenine(Bin) -> {?BadOneNine, ?IOEMP, Bin}.

sign(<<$+, Tail>>) -> {?OK, ?IOLST(<<$+>>), Tail};
sign(<<$-, Tail>>) -> {?OK, ?IOLST(<<$->>), Tail};
sign(Bin) -> {?OK, ?IOEMP, Bin}.

ws(<<16#9, Tail>>) -> {?OK, ?IOLST(<<16#9>>), Tail};
ws(<<16#a, Tail>>) -> {?OK, ?IOLST(<<16#a>>), Tail};
ws(<<16#d, Tail>>) -> {?OK, ?IOLST(<<16#d>>), Tail};
ws(<<16#20, Tail>>) -> {?OK, ?IOLST(<<16#20>>), Tail};
ws(Bin) -> {?OK, ?IOEMP, Bin}.
