-module(grid_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Macros --------------------------------------------------------------------

-define(equal(Rows, Expr),
    ?assertEqual(
        string:trim(iolist_to_binary([[R, $\n] || R <- Rows])),
        string:trim(iolist_to_binary(grid:format(Expr)))
    )
).

%--- Tests ---------------------------------------------------------------------

empty_test() -> ?equal("", []).

simple_test() ->
    ?equal(
        [
            "1    3.14  foo",
            "bar  2     baz"
        ],
        [
            [1, "3.14", foo],
            [bar, 2, <<"baz">>]
        ]
    ).
