-module(grid).

% API
-export([format/1]).
-ignore_xref({format, 1}).
-export([format/2]).
-ignore_xref({format, 2}).

%--- API -----------------------------------------------------------------------

format(Items) -> format(Items, #{}).

format(Items, Opts) ->
    {Rows, Columns} = process(Items, columns(maps:get(columns, Opts, []))),
    render(Rows, Columns, Opts).

%--- Internal ------------------------------------------------------------------

process(Items, Columns) ->
    {Rows, AllColumns} = process(Items, Columns, []),
    {Rows, filter_columns(Columns, AllColumns)}.

process([], Columns, Items) ->
    {lists:reverse(Items), lists:sort(fun(#{index := I1}, #{index := I2}) -> I1 =< I2 end, column_index(Columns))};
process([Item | Rest], Columns, Items) ->
    {Formatted, NewColumns} = process_item(Item, Columns),
    process(Rest, NewColumns, [Formatted | Items]).

process_item(Item, Columns) when is_list(Item) ->
    process_item_list(Item, 1, {[], Columns});
process_item(Item, Columns) when is_map(Item) ->
    process_item_map(Item, Columns).

process_item_list([], _Pos, {Item, Columns}) ->
    {lists:reverse(Item), Columns};
process_item_list([Cell | Rest], Pos, {Item, Columns}) ->
    {Rendered, Length} = render_cell(Cell),
    NewColumns = update_columns(index, Pos, #{width => Length}, Columns),
    process_item_list(Rest, Pos + 1, {[Rendered | Item], NewColumns}).

process_item_map(Cells, Columns) ->
    maps:fold(fun(Key, Value, {Acc, Cs}) ->
        {Rendered, Length} = render_cell(Value),
        NewColumns = update_columns(key, Key, #{width => Length, key => Key}, Cs),
        {Acc#{Key := Rendered}, NewColumns}
    end, {Cells, Columns}, Cells).

update_columns(Key, Value, Updates, []) ->
    [maps:put(Key, Value, Updates)];
update_columns(Key, Value, Updates, [Column|Columns]) ->
    case Column of
        #{Key := Value} ->
            [maps:merge_with(
                fun
                    (width, W1, W2) -> max(W1, W2);
                    (_, _, V) -> V
                end,
                Column,
                Updates
            )|Columns];
        Other ->
            [Other|update_columns(Key, Value, Updates, Columns)]
    end.

render(Items, Columns, Opts) ->
    lists:map(fun(Item) -> render_row(Item, Columns, Opts) end, Items).

render_row(Item, Columns, Opts) when is_list(Item) ->
    render_row_list(Item, Columns, Opts);
render_row(Item, Columns, Opts) when is_map(Item) ->
    render_row_map(Item, Columns, Opts).

render_row_list([], [], _Opts) ->
    [$\n];
render_row_list([Cell | Row], [#{width := Width} | Columns], Opts) ->
    Formatted = io_lib:format("~*.. s", [-Width, Cell]),
    [[Formatted | [spacer(Opts) || Row =/= []]] | render_row_list(Row, Columns, Opts)].

render_row_map(Row, Columns, Opts) when is_map(Row) ->
    [lists:join(spacer(Opts), lists:foldr(fun(#{key := Key, width := Width}, Acc) ->
        Formatted = io_lib:format("~*.. s", [-Width, maps:get(Key, Row)]),
        [Formatted | Acc]
    end, [], Columns)), $\n].

render_cell(Term) ->
    Rendered = render_cell_value(Term),
    {Rendered, string:length(Rendered)}.

render_cell_value(Term) when is_binary(Term); is_list(Term) ->
    Term;
render_cell_value(Term) when is_integer(Term) ->
    integer_to_binary(Term);
render_cell_value(Term) when is_atom(Term) ->
    atom_to_binary(Term).

columns(Columns) -> columns(Columns, 1, []).

columns([], _Pos, Acc) ->
    lists:reverse(Acc);
columns([Column|Columns], Pos, Acc) ->
    columns(Columns, Pos + 1, [#{key => Column, index => Pos}|Acc]).

column_index([#{index := Index} = Column|Columns]) ->
    [Column|column_index(Columns, Index)];
column_index(Columns) ->
    column_index(Columns, 1).

column_index(Columns, Index) ->
    {Indexed, _} = lists:mapfoldl(
        fun
            (#{index := I} = Column, I) -> {Column, I + 1};
            (#{index := I2}, I2) -> error({index_mismatch, I2, I2, Columns});
            (Column, I) -> {Column#{index => I}, I + 1}
        end,
        Index,
        Columns
    ),
    Indexed.

filter_columns([], Columns) ->
    Columns;
filter_columns(Desired, All) ->
    lists:sublist(All, length(Desired)).

spacer(#{spacer := Spacer}) -> Spacer;
spacer(_Opts) -> <<"  ">>.
