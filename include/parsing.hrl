-ifndef(__parsing_hrl).
-define(__parsing_hrl, __parsing_hrl).

-type parsed() :: any().
-type input() :: string().

-record(parsing, { parsed :: parsed(),
		   rest :: input() }).

-type parser() :: fun((input()) -> [#parsing{}]).

-endif.
