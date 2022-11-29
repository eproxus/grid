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

list_test() ->
    ?equal(
        [
            "1",
            "2  foo   c",
            "3  quux"
        ],
        [
            [1],
            [2, foo, "c"],
            [3, <<"quux">>]
        ]
    ).

tuple_test() ->
    ?equal(
        [
            "1",
            "2  foo   c",
            "3  quux"
        ],
        [
            {1},
            {2, foo, "c"},
            {3, <<"quux">>}
        ]
    ).

maps_test() ->
    ?equal(
        [
            "1",
            "2  c  bar",
            "3     bar"
        ],
        [
            #{type => 1},
            #{type => 2, value => bar, extra => "c"},
            #{type => 3, value => <<"bar">>}
        ]
    ).

mixed_rows_test() ->
    ?equal(
        [
            "1",
            "2  foo   c",
            "3  quux"
        ],
        [
            #{type => 1},
            [2, foo, "c"],
            {3, <<"quux">>}
        ]
    ).

empty_columns_test() ->
    ?assertError(
        empty_columns,
        grid:format([[a], [b]], #{columns => []})
    ).

maps_column_test() ->
    ?equal(
        [
            "foobar  1",
            "baz     295"
        ],
        [
            #{type => foobar, value => 1},
            #{type => baz, value => 295}
        ],
        #{columns => [type, value]}
    ).

maps_column_spec_test() ->
    ?equal(
        [
            "1    foobar",
            "295  baz"
        ],
        [
            #{type => foobar, value => 1},
            #{type => baz, value => 295}
        ],
        #{columns => [#{key => value}, type]}
    ).

maps_column_disorder_test() ->
    ?equal(
        [
            "2  3  1",
            "5  6  4"
        ],
        [
            #{a => 1, b => 2, c => 3},
            #{a => 4, b => 5, c => 6}
        ],
        #{
            columns => [
                #{key => a, index => 3},
                #{key => b, index => 1},
                #{key => c, index => 2}
            ]
        }
    ).

maps_column_same_index_test() ->
    ?assertError(
        {duplicate_index, _, _},
        grid:format(
            [
                #{type => foobar, value => 1},
                #{type => baz, value => 295}
            ],
            #{
                columns => [
                    #{key => value, index => 1}, #{key => type, index => 1}
                ]
            }
        )
    ).

duplicate_column_key_test() ->
    ?equal(
        [
            "1  1  2  2  3",
            "4  4  5  5  6"
        ],
        [
            #{a => 1, b => 2, c => 3},
            #{a => 4, b => 5, c => 6}
        ],
        #{
            columns => [
                #{key => a},
                #{key => a},
                #{key => b},
                #{key => b},
                #{key => c}
            ]
        }
    ).

duplicate_column_index_test() ->
    ?equal(
        [
            "3  2  1  2  3",
            "6  5  4  5  6"
        ],
        [
            [1, 2, 3],
            [4, 5, 6]
        ],
        #{
            columns => [
                #{index => 3},
                #{index => 2},
                #{index => 1},
                #{index => 2},
                #{index => 3}
            ]
        }
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

empty_header_test() -> ?equal([], [], #{header => true}).

implicit_header_test() ->
    ?equal(
        [
            "1  2  3",
            "a",
            "a  b  c",
            "a  b"
        ],
        [
            [a],
            [a, b, c],
            [a, b]
        ],
        #{header => true}
    ).

column_header_test() ->
    ?equal(
        [
            "value  type",
            "1      foobar",
            "baz    295"
        ],
        [
            #{type => foobar, value => 1},
            [baz, 295]
        ],
        #{header => true, columns => [value, type]}
    ).

column_name_test() ->
    ?equal(
        [
            "VALUE  TYPE",
            "1      foobar",
            "baz    295"
        ],
        [
            #{type => foobar, value => 1},
            [baz, 295]
        ],
        #{
            header => true,
            columns => [
                #{key => value, name => "VALUE"},
                #{key => type, name => "TYPE"}
            ]
        }
    ).

column_align_test() ->
    ?equal(
        [
            "VALUE   TYPE",
            "    1  foobar",
            "  baz   295"
        ],
        [
            #{type => foobar, value => 1},
            [baz, 295]
        ],
        #{
            header => true,
            columns => [
                #{key => value, name => "VALUE", align => right},
                #{key => type, name => "TYPE", align => center}
            ]
        }
    ).

header_format_test() ->
    ?equal(
        [
            "FOO     BAZ",
            "foobar  quuuux"
        ],
        [#{foo => foobar, bar => bar, baz => quuuux}],
        #{header => uppercase, columns => [foo, baz]}
    ).

header_format_only_test_() ->
    Test = fun(Key, Expected, Format) ->
        fun() ->
            ?equal(
                [
                    Expected,
                    "x"
                ],
                [#{Key => x, other => y}],
                #{
                    header => #{format => Format},
                    columns => [Key]
                }
            )
        end
    end,
    {inparallel, [
        Test(foo_bar, "FOO BAR", uppercase),
        Test(foo_bar, "Foo bar", titlecase),
        Test('FOO_BAR', "foo bar", lowercase),
        Test('FOO_BAR', "RAB_OOF", fun(S) -> string:reverse(S) end),
        Test('FOO_BAR', "RAB_OOF", fun(S) ->
            grid:cell(string:reverse(S), string:length(S))
        end)
    ]}.

%--- Internal ------------------------------------------------------------------

bintrim(IOList) -> string:trim(iolist_to_binary(IOList)).

rows(Rows) -> bintrim([[R, $\n] || R <- Rows]).
