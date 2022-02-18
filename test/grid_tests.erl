-module(grid_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Macros --------------------------------------------------------------------

-define(equal(Rows, Expr),
    ?assertEqual(rows(Rows), bintrim(grid:format(Expr)))
).
-define(equal(Rows, Expr, Opts),
    ?assertEqual(rows(Rows), bintrim(grid:format(Expr, Opts)))
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

maps_test() ->
    ?equal(
        [
            "foobar  1  ",
            "baz     295"
        ],
        [
            #{type => foobar, value => 1},
            #{type => baz, value => 295}
        ]
    ).

maps_column_test() ->
    ?equal(
        [
            "foobar  1  ",
            "baz     295"
        ],
        [
            #{type => foobar, value => 1},
            #{type => baz, value => 295}
        ],
        #{columns => [type, value]}
    ).

mixed_rows_test() ->
    ?equal(
        [
            "1    foobar",
            "baz  295   "
        ],
        [
            #{type => foobar, value => 1},
            [baz, 295]
        ],
        #{columns => [value, type]}
    ).

custom_spacer_test() ->
    ?equal(
        [
            "1  |3.14|foo",
            "bar|2   |baz"
        ],
        [
            [1, "3.14", foo],
            [bar, 2, <<"baz">>]
        ],
        #{spacer => $|}
    ).


%--- Internal ------------------------------------------------------------------

bintrim(IOList) -> string:trim(iolist_to_binary(IOList)).

rows(Rows) -> bintrim([[R, $\n] || R <- Rows]).
