-module(grid).

% API
-export([format/1]).
-ignore_xref({format, 1}).
-export([format/2]).
-ignore_xref({format, 2}).

%--- Macros --------------------------------------------------------------------

-define(cell(Item, Length), {'$cell', Length, Item}).

%--- API -----------------------------------------------------------------------

format(Items) -> format(Items, #{}).

format(Items, Opts) ->
    {Rows, Columns} = process(Items, columns(Opts), opts(Opts)),
    render(Rows, Columns, Opts).

%--- Internal ------------------------------------------------------------------

opts(Opts) -> maps:map(fun opt/2, Opts).

opt(header, true) -> #{format => fun(V) -> V end};
opt(header, Format) when is_atom(Format) -> #{format => Format};
opt(columns, []) -> error(empty_columns);
opt(_K, V) -> V.

columns(Opts) -> columns(maps:get(columns, Opts, []), 1, #{}).

columns([], Index, Acc) ->
    {Index, Acc};
columns([Column | Columns], Index, Acc) when is_atom(Column) ->
    NewAcc = maps:put(Index, #{key => Column, index => Index}, Acc),
    columns(Columns, Index + 1, NewAcc);
columns([I | Columns], Index, Acc) when is_integer(I) ->
    NewAcc = maps:put(I, #{index => Index}, Acc),
    columns(Columns, I, NewAcc);
columns([#{index := I} = Column | Columns], _Index, Acc) ->
    case maps:find(I, Acc) of
        {ok, Existing} -> error({duplicate_index, Column, Existing});
        error -> columns(Columns, I + 1, maps:put(I, Column, Acc))
    end;
columns([Column | Columns], Index, Acc) when is_map(Column) ->
    columns(Columns, Index + 1, maps:put(Index, Column#{index => Index}, Acc)).

process(Items, Columns, Opts) ->
    {Rows, AllColumns} = process(Items, Columns, [], Opts),
    {Rows, filter_columns(Columns, AllColumns)}.

process([], Columns, Rows, Opts) ->
    {Header, {_Index, NewColumns}} = process_headers(Columns, Opts),
    AllRows = Header ++ lists:reverse(Rows),
    IndexCompare = fun(#{index := I1}, #{index := I2}) -> I1 =< I2 end,
    SortedColumns = lists:sort(IndexCompare, maps:values(NewColumns)),
    {AllRows, SortedColumns};
process([Item | Rest], Columns, Rows, Opts) ->
    {Row, NewColumns} = process_row(Item, Columns),
    process(Rest, NewColumns, [Row | Rows], Opts).

process_headers({Index, Columns}, #{header := Header}) ->
    Process = fun(_, Column, Acc) -> process_header(Header, Column, Acc) end,
    Item = lists:reverse(maps:fold(Process, [], Columns)),
    {Formatted, NewColumns} = process_row(Item, {Index, Columns}),
    {[Formatted], NewColumns};
process_headers(Columns, _Opts) ->
    {[], Columns}.

process_header(Header, #{name := Name}, Acc) ->
    [format_header(Header, Name) | Acc];
process_header(Header, #{key := Key}, Acc) ->
    [format_header(Header, render_cell_value(Key)) | Acc];
process_header(Header, #{index := I}, Acc) ->
    [format_header(Header, render_cell_value(I)) | Acc].

process_row(Row, Columns) ->
    process_row(next(iter(Row)), Columns, {}).

process_row(none, Columns, Acc) ->
    {Acc, Columns};
process_row({Item, Pos, Iter}, Columns, Acc) ->
    Cell = process_cell(Item),
    {Index, NewColumns} =
        update_columns(Pos, #{width => cell_length(Cell)}, Columns),
    NewAcc = set_cell(Acc, Index, Cell),
    process_row(next(Iter), NewColumns, NewAcc).

cell_length(?cell(_Text, Length)) -> Length.

set_cell(Row, Index, Cell) when Index =< tuple_size(Row) ->
    setelement(Index, Row, Cell);
set_cell(Row, Index, Cell) when Index == tuple_size(Row) + 1 ->
    erlang:append_element(Row, Cell);
set_cell(Row, Index, Cell) ->
    set_cell(erlang:append_element(Row, '_'), Index, Cell).

iter(Item) when is_list(Item) -> {list, 1, Item};
iter(Item) when is_tuple(Item) -> {tuple, 1, Item};
iter(Item) when is_map(Item) -> {map, 1, maps:next(maps:iterator(Item))};
iter(Item) -> error({unknown_row_type, Item}).

next({list, _Index, []}) ->
    none;
next({list, Index, [Elem | Rest]}) ->
    {Elem, {index, Index}, {list, Index + 1, Rest}};
next({tuple, Index, Tuple}) when Index > tuple_size(Tuple) ->
    none;
next({tuple, Index, Tuple}) ->
    {element(Index, Tuple), {index, Index}, {tuple, Index + 1, Tuple}};
next({map, _Index, none}) ->
    none;
next({map, Index, {Key, Value, NewIter}}) ->
    {Value, {key, Key}, {map, Index + 1, NewIter}}.

process_cell(?cell(Term, Length)) ->
    ?cell(render_cell_value(Term), Length);
process_cell(Term) ->
    Rendered = render_cell_value(Term),
    ?cell(Rendered, string:length(Rendered)).

update_columns({index, Value}, Attrs, {Index, Columns}) ->
    NewColumns =
        case maps:find(Value, Columns) of
            {ok, Column} ->
                {Index, maps:put(Value, update_column(Column, Attrs), Columns)};
            error ->
                {Index + 1, maps:put(Index, Attrs#{index => Index}, Columns)}
        end,
    {Value, NewColumns};
update_columns({Key, Value}, Attrs, {Index, Columns}) ->
    First = maps:next(maps:iterator(Columns)),
    update_columns(Key, Value, Attrs, {Index, Columns}, First).

update_columns(Key, Value, Attrs, {Index, Columns}, none) ->
    NewColumns = maps:put(Index, Attrs#{Key => Value, index => Index}, Columns),
    {Index, {Index + 1, NewColumns}};
update_columns(Key, Value, Attrs, {Index, Columns}, {I, Column, Iter}) ->
    case Column of
        #{Key := Value, index := I} ->
            {I, {Index, maps:put(I, update_column(Column, Attrs), Columns)}};
        _Other ->
            update_columns(Key, Value, Attrs, {Index, Columns}, maps:next(Iter))
    end.

update_column(Column, Updates) ->
    Update = fun
        (width, W1, W2) -> max(W1, W2);
        (_, _, V) -> V
    end,
    % FIXME: When supporting OTP 24+ only, use maps:merge_with/3 instead
    % maps:merge_with(Update, Column, Updates).
    maps:fold(
        fun(Key, Value, C) ->
            case maps:find(Key, C) of
                {ok, Old} -> C#{Key := Update(Key, Old, Value)};
                error -> C#{Key => Value}
            end
        end,
        Column,
        Updates
    ).

render(Items, Columns, Opts) ->
    lists:map(fun(Item) -> render_row(Item, Columns, Opts) end, Items).

render_row({}, _Columns, _Opts) ->
    [$\n];
render_row({Cell}, [Column | _], _Opts) ->
    [render_cell(Cell, Column, no_padding), $\n];
render_row(Row, [Column], _Opts) ->
    [render_cell(element(1, Row), Column, no_padding), $\n];
render_row(Row, [Column | Columns], Opts) ->
    Rendered = render_cell(element(1, Row), Column, padding),
    [Rendered, render_cells(Row, 2, Columns, Opts), $\n].

render_cells(_Row, _Index, [], _Opts) ->
    [];
render_cells(Row, Index, [Column | _], Opts) when Index == tuple_size(Row) ->
    [spacer(Opts), render_cell(element(Index, Row), Column, no_padding)];
render_cells(Row, Index, [Column], Opts) ->
    [spacer(Opts), render_cell(element(Index, Row), Column, no_padding)];
render_cells(Row, Index, [Column | Columns], Opts) ->
    Rendered = render_cell(element(Index, Row), Column, padding),
    [spacer(Opts), Rendered | render_cells(Row, Index + 1, Columns, Opts)].

render_cell('_', _Column, no_padding) ->
    [];
render_cell('_', #{width := CW}, padding) ->
    lists:duplicate(CW, $\s);
render_cell(?cell(Text, W), #{width := CW}, padding) when W =< CW ->
    [Text, lists:duplicate(CW - W, $\s)];
render_cell(?cell(Text, _W), _Column, no_padding) ->
    [Text].

render_cell_value(Term) when is_binary(Term); is_list(Term) ->
    Term;
render_cell_value(Term) when is_integer(Term) ->
    integer_to_binary(Term);
render_cell_value(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8).

filter_columns({_, Desired}, Columns) when map_size(Desired) == 0 -> Columns;
filter_columns({_, Desired}, All) -> lists:sublist(All, maps:size(Desired)).

spacer(#{spacer := Spacer}) -> Spacer;
spacer(_Opts) -> <<"  ">>.

format_header(#{format := Format}, Header) ->
    format_text(Format, Header).

format_text(uppercase, Binary) -> string:uppercase(words(Binary));
format_text(titlecase, Binary) -> string:titlecase(words(Binary));
format_text(lowercase, Binary) -> string:lowercase(words(Binary));
format_text(Fun, Binary) when is_function(Fun, 1) -> Fun(Binary);
format_text(Format, _Binary) -> error({invalid_format, Format}).

words(Binary) -> string:replace(Binary, <<"_">>, <<" ">>, all).
