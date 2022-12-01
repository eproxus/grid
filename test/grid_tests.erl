-module(grid_tests).

-include_lib("eunit/include/eunit.hrl").

%--- Macros --------------------------------------------------------------------

-define(printResult(Rows, Result), begin
    __Result = Result,
    io:format("~s~n---~n~s", [lists:join($\n, Rows), __Result]),
    __Result
end).
-define(equal(Rows, Expr),
    ?assertEqual(Rows, rows(?printResult(Rows, grid:format(Expr))))
).
-define(equal(Rows, Expr, Opts),
    ?assertEqual(Rows, rows(?printResult(Rows, grid:format(Expr, Opts))))
).

%--- Tests ---------------------------------------------------------------------

empty_test() -> ?equal("", []).

implicit_row_test_() ->
    Test = fun(Term) -> fun() -> ?equal([to_list(Term)], [Term]) end end,
    {inparallel, [
        Test(Term)
     || Term <- [
            foobar,
            1,
            3.14,
            <<"foobar">>,
            self(),
            make_ref()
        ]
    ]}.

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

map_test() ->
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

proplist_test() ->
    ?equal(
        [
            "1",
            "2         c          bar",
            "3                    bar",
            "{type,4}  {value,c}  bar"
        ],
        [
            [{type, 1}],
            [{type, 2}, {value, bar}, {extra, "c"}],
            [{type, 3}, {value, <<"bar">>}],
            [{type, 4}, {value, c}, bar]
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

mixed_columns_test() ->
    ?equal(
        [
            "   1",
            "2  foo   c",
            "3  quux"
        ],
        [
            #{type => 1},
            [2, foo, "c"],
            {3, <<"quux">>}
        ],
        #{columns => [1, #{key => type}, #{index => 3}]}
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
                #{key => b},
                #{key => c},
                #{key => a}
            ]
        }
    ).

tuple_column_disorder_test() ->
    ?equal(
        [
            "1  3  2",
            "4  6  5"
        ],
        [
            {1, 2, 3},
            {4, 5, 6}
        ],
        #{
            columns => [
                #{},
                #{index => 3},
                #{index => 2}
            ]
        }
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
            "VALUE          TYPE COLUMN",
            "1      foobar  foobar       foobar",
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
                #{key => type, name => <<>>},
                #{key => type, name => "TYPE COLUMN"},
                #{key => type, name => ""}
            ]
        }
    ).

column_align_test() ->
    ?equal(
        [
            "VALUE   TYPE      TAG",
            "    1  foobar  #big-item",
            "  baz   295      #item"
        ],
        [
            #{type => foobar, value => 1, tag => "#big-item"},
            [baz, 295, "#item"]
        ],
        #{
            header => true,
            columns => [
                #{key => value, name => "VALUE", align => right},
                #{key => type, name => "TYPE", align => center},
                #{index => 3, key => tag, name => "TAG", align => center}
            ]
        }
    ).

column_format_test() ->
    Format = fun(Call) ->
        fun
            (Term) when is_atom(Term) -> Call(atom_to_binary(Term));
            (Term) when is_integer(Term) -> Call(integer_to_binary(Term))
        end
    end,
    ?equal(
        [
            "1    raboof",
            "BAZ  592"
        ],
        [
            #{type => foobar, value => 1},
            [baz, 295]
        ],
        #{
            columns => [
                #{key => value, format => uppercase},
                #{key => type, format => Format(fun string:reverse/1)}
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
        Test('FOO_BAR', "RAB_OOF", fun(Value) ->
            string:reverse(atom_to_binary(Value))
        end),
        Test('FOO_BAR', "RAB_OOF", fun(Value) ->
            Formatted = string:reverse(atom_to_binary(Value)),
            grid:cell(Formatted, string:length(Formatted))
        end)
    ]}.

invalid_format_test() ->
    ?assertError(
        {invalid_format, foobar},
        grid:format([1], #{header => foobar})
    ),
    ?assertError(
        {invalid_format, foobar},
        grid:format([1], #{columns => [#{format => foobar}]})
    ).

invalid_formatter_test() ->
    ?assertError(
        {invalid_format_return, foobar},
        grid:format(
            [[foobar]],
            #{columns => [#{format => fun(Value) -> Value end}]}
        )
    ).

%--- Internal ------------------------------------------------------------------

rows(IOList) -> string:lexemes(binary_to_list(iolist_to_binary(IOList)), [$\n]).

to_list(Term) when is_binary(Term) -> binary_to_list(Term);
to_list(Term) -> lists:flatten(io_lib:format("~p", [Term])).
