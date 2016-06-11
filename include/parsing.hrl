-ifndef(__parsing_hrl).
-define(__parsing_hrl, __parsing_hrl).

-type to_parse(T) :: T.
-type parsed(T) :: T.
-type input(T) :: T.
-type predicate(T) :: fun((T) -> boolean()).
-type literal(T) :: T.

-record(parsing, { parsed, rest }).

-type parsing(A, B) :: #parsing{ parsed :: A, rest :: B }.
-type parser(A, B) :: fun((input(A)) -> [parsing(B, A)]).

-type function(A, B) :: fun((A) -> B).

-endif.
