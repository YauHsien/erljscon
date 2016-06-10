-ifndef(__parsing_hrl).
-define(__parsing_hrl, __parsing_hrl).

-type parsed() :: any().
-type input() :: string().
-type predicate() :: fun((any()) -> boolean()).

-record(parsing, { parsed :: parsed(),
		   rest :: input() }).

-type parsing() :: #parsing{}.
-type parser() :: fun((input()) -> [parsing()]).

-endif.
