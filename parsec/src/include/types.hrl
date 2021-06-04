-type parser(FromType, ToType) :: fun(([FromType]) -> [{ToType, [FromType]}]).
-type from() :: any().
-type to() :: any().
-type 'case'() :: any().
-define(REC(X),
        {rec, fun()-> X end}).
-type pos(Type) :: {Type,{integer(),integer()}}.
-type tag() :: 'Ident' | 'Number' | 'Symbol' | 'Junk'.
-type token() :: {tag(),[char()]}.
