-module(grid).

% API
-export([format/1]).
-ignore_xref({format, 1}).
-export([format/2]).
-ignore_xref({format, 2}).
-export([cell/2]).
-ignore_xref({cell, 2}).

%--- Macros --------------------------------------------------------------------

-define(cell(Item, Length), {'$grid:cell', Length, Item}).

-define(COLUMN_DEFAULT, #{align => left}).

%--- API -----------------------------------------------------------------------

format(Items) -> format(Items, #{}).

format(Items, Opts) ->
    {Rows, Columns} = process(Items, columns(Opts), opts(Opts)),
    render(Rows, Columns, Opts).

cell(Item, Length) -> ?cell(Item, Length).

%--- Internal ------------------------------------------------------------------

opts(Opts) -> maps:map(fun opt/2, Opts).

opt(header, true) -> #{format => fun(V) -> V end};
opt(header, Format) when is_atom(Format) -> #{format => Format};
opt(columns, []) -> error(empty_columns);
opt(_K, V) -> V.

columns(#{columns := Columns}) -> columns(Columns, 1, #{});
columns(_Opts) -> #{total => 1, specs => #{}, detect => true}.

columns([], Index, Acc) ->
    #{total => Index, specs => Acc};
columns([Column | Columns], Index, Acc) when is_atom(Column) ->
    NewAcc = Acc#{Index => #{key => Column, index => Index}},
    columns(Columns, Index + 1, NewAcc);
columns([I | Columns], Index, Acc) when is_integer(I) ->
    NewAcc = Acc#{I => #{index => Index}},
    columns(Columns, max(I + 1, Index), NewAcc);
columns([#{index := I} = Column | Columns], Index, Acc) ->
    case maps:find(I, Acc) of
        {ok, Existing} -> error({duplicate_index, Column, Existing});
        error -> columns(Columns, max(I + 1, Index), Acc#{I => column(Column)})
    end;
columns([Column | Columns], Index, Acc) when is_map(Column) ->
    columns(Columns, Index + 1, Acc#{Index => column(Column#{index => Index})}).

process(Items, Columns, Opts) ->
    {Rows, AllColumns} = process(Items, Columns, [], Opts),
    {Rows, filter_columns(Columns, AllColumns)}.

process([], Columns, Rows, Opts) ->
    {Header, NewColumns} = process_headers(Columns, Opts),
    {Header ++ lists:reverse(Rows), NewColumns};
process([Item | Rest], Columns, Rows, Opts) ->
    {Row, NewColumns} = process_row(Item, Columns),
    process(Rest, NewColumns, [Row | Rows], Opts).

process_headers(#{specs := Specs} = Columns, #{header := Header}) ->
    Process = fun(_, Column, Acc) -> process_header(Header, Column, Acc) end,
    Item = lists:reverse(maps:fold(Process, [], Specs)),
    {Formatted, NewColumns} = process_row(Item, Columns),
    {[Formatted], NewColumns};
process_headers(Columns, _Opts) ->
    {[], Columns}.

process_header(Header, #{name := Name}, Acc) ->
    [format_header(Header, render_cell_value(Name)) | Acc];
process_header(Header, #{key := Key}, Acc) ->
    [format_header(Header, render_cell_value(Key)) | Acc];
process_header(Header, #{index := I}, Acc) ->
    [format_header(Header, render_cell_value(I)) | Acc].

process_row(Row, #{detect := true} = Columns) ->
    process_row_by_cells(next(iter(Row)), Columns, {});
process_row(Row, Columns) ->
    process_row_by_columns(Row, Columns, 1, {}).

process_row_by_cells(none, Columns, Acc) ->
    {Acc, Columns};
process_row_by_cells({Item, Pos, Iter}, Columns, Acc) ->
    Cell = process_cell(Item),
    {Index, NewColumns} =
        update_columns(Pos, #{width => cell_length(Cell)}, Columns),
    NewAcc = set_cell(Acc, Index, Cell),
    process_row_by_cells(next(Iter), NewColumns, NewAcc).

process_row_by_columns(_Row, #{total := Total} = Columns, Index, Acc) when
    Index >= Total
->
    {Acc, Columns};
process_row_by_columns(Row, #{specs := Specs} = Columns, Index, Acc) ->
    Spec = maps:get(Index, Specs),
    Processed = process_cell(get_cell(Row, Spec)),
    NewAcc = set_cell(Acc, Index, Processed),
    {_, NewColumns} =
        update_columns(
            {index, maps:get(index, Spec)},
            #{width => cell_length(Processed)},
            Columns
        ),
    process_row_by_columns(Row, NewColumns, Index + 1, NewAcc).

get_cell(Item, #{index := Index}) when is_list(Item) ->
    lists:nth(Index, Item);
get_cell(Item, #{index := Index}) when is_tuple(Item) ->
    element(Index, Item);
get_cell(Item, #{key := Key}) when is_map(Item) ->
    maps:get(Key, Item);
get_cell(Item, _Column) ->
    error({unknown_row_type, Item}).

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

update_columns(
    {index, Value}, Attrs, #{total := Total, specs := Specs} = Columns
) ->
    {NewTotal, NewSpecs} =
        case maps:find(Value, Specs) of
            {ok, Column} ->
                {Total, Specs#{Value => update_column(Column, Attrs)}};
            error ->
                {Total + 1, Specs#{Total => column(Attrs#{index => Total})}}
        end,
    {Value, Columns#{total := NewTotal, specs := NewSpecs}};
update_columns({Key, Value}, Attrs, #{specs := Specs} = Columns) ->
    First = maps:next(maps:iterator(Specs)),
    update_columns(Key, Value, Attrs, Columns, First).

update_columns(
    Key, Value, Attrs, #{total := Total, specs := Specs} = Columns, none
) ->
    NewSpecs = Specs#{Total => column(Attrs#{Key => Value, index => Total})},
    {Total, Columns#{total := Total + 1, specs := NewSpecs}};
update_columns(
    Key, Value, Attrs, #{specs := Specs} = Columns, {I, Column, Iter}
) ->
    case Column of
        #{Key := Value, index := I} ->
            {I, Columns#{specs := Specs#{I => update_column(Column, Attrs)}}};
        _Other ->
            update_columns(Key, Value, Attrs, Columns, maps:next(Iter))
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
                error -> maps:merge(?COLUMN_DEFAULT, C#{Key => Value})
            end
        end,
        Column,
        Updates
    ).

column(Attrs) -> maps:merge(?COLUMN_DEFAULT, Attrs).

render(Items, #{specs := Specs}, Opts) ->
    IndexCompare = fun(#{index := I1}, #{index := I2}) -> I1 =< I2 end,
    Sorted = lists:sort(IndexCompare, maps:values(Specs)),
    lists:map(fun(Item) -> render_row(Item, Sorted, Opts) end, Items).

render_row({}, _Columns, _Opts) ->
    [$\n];
render_row({Cell}, [Column | _], _Opts) ->
    [render_cell(Cell, Column, no_pad), $\n];
render_row(Row, [Column], _Opts) ->
    [render_cell(element(1, Row), Column, no_pad), $\n];
render_row(Row, [Column | Columns], Opts) ->
    Rendered = render_cell(element(1, Row), Column, right_pad),
    [Rendered, render_cells(Row, 2, Columns, Opts), $\n].

render_cells(_Row, _Index, [], _Opts) ->
    [];
render_cells(Row, Index, [Column | _], Opts) when Index == tuple_size(Row) ->
    [spacer(Opts), render_cell(element(Index, Row), Column, no_pad)];
render_cells(Row, Index, [Column], Opts) ->
    [spacer(Opts), render_cell(element(Index, Row), Column, no_pad)];
render_cells(Row, Index, [Column | Columns], Opts) ->
    Rendered = render_cell(element(Index, Row), Column, right_pad),
    [spacer(Opts), Rendered | render_cells(Row, Index + 1, Columns, Opts)].

render_cell('_', _Column, no_pad) ->
    [];
render_cell('_', #{width := CW}, right_pad) ->
    lists:duplicate(CW, $\s);
render_cell(?cell(Text, Width), #{width := CWidth, align := Align}, Pad) ->
    render_padding(Text, Width, CWidth, Align, Pad).

render_padding(Text, W, CW, left, right_pad) when W =< CW ->
    [Text, lists:duplicate(CW - W, $\s)];
render_padding(Text, W, CW, right, _) when W =< CW ->
    [lists:duplicate(CW - W, $\s), Text];
render_padding(Text, W, CW, center, right_pad) when W =< CW ->
    Pad = (CW - W),
    Left = Pad div 2,
    Right = Pad - Left,
    [lists:duplicate(Left, $\s), Text, lists:duplicate(Right, $\s)];
render_padding(Text, W, CW, center, no_pad) ->
    Pad = (CW - W),
    Left = Pad div 2,
    [lists:duplicate(Left, $\s), Text];
render_padding(Text, _W, _CW, _Align, no_pad) ->
    Text.

render_cell_value(Term) when is_binary(Term); is_list(Term) ->
    Term;
render_cell_value(Term) when is_integer(Term) ->
    integer_to_binary(Term);
render_cell_value(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
render_cell_value(Term) ->
    error({invalid_cell, Term}).

filter_columns(#{detect := true}, Columns) ->
    Columns;
filter_columns(#{specs := Specs}, Columns) when map_size(Specs) == 0 ->
    Columns;
filter_columns(#{specs := Desired} = Columns, #{specs := Detected}) ->
    Specs = maps:map(
        fun(Index, Spec) ->
            maps:merge(Spec, maps:get(Index, Detected))
        end,
        Desired
    ),
    Columns#{specs := Specs}.

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
