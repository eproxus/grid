% @doc Formatting library for tabular data.
-module(grid).

% API
-export([format/1]).
-ignore_xref({format, 1}).
-export([format/2]).
-ignore_xref({format, 2}).
-export([cell/2]).
-ignore_xref({cell, 2}).

%--- Macros --------------------------------------------------------------------

-define(COLUMN_DEFAULT, #{align => left, format => fun format_default/1}).

%--- Records -------------------------------------------------------------------

-record(cell, {
    text :: iodata(),
    width :: non_neg_integer()
}).

%--- Types ---------------------------------------------------------------------

-type format_fun() :: fun((term()) -> iodata() | cell()).
% Function that takes a cell value and return its textual representation.
% Either as printable IO-data or a {@type cell()}.
-type format() :: uppercase | lowercase | titlecase | format_fun().
% A format specification, either as a shortcut atom or a custom fun of type
% {@type format_fun()}.
-type header_opt() :: boolean() | format().
% Enable headers by setting to `true' or supplying a specific format of type
% {@type format()}. Setting it to `false' disables printing of headers.
-type column() ::
    pos_integer()
    | atom()
    | #{
        index => pos_integer(),
        key => atom(),
        name => iodata(),
        format => format()
    }.
% Column display specification. An integer `Integer' is equivalent to
% `#{index => Integer}', an atom `Atom' is equivalent to `#{key => Atom}'.
%
% <ul>
%   <li>`index' is which cell index should be rendered in this column</li>
%   <li>`key' is which cell key should be rendered in this column</li>
%   <li>`name' is the display name to use for the column header</li>
%   <li>`format' is the format specification that will be applied to each
%       cell</li>
% </ul>
-type columns() :: [column()].
% Ordered list of columns to print.
-type opts() :: #{
    header => header_opt(),
    spacer => iodata(),
    columns => columns()
}.
% Options to customize printing.
%
% <ul>
%   <li>`spacer' defaults to `"  "'</li>
%   <li>`header' defaults to `false'</li>
%   <li>`columns' defaults to to printing all columns as-is</li>
% </ul>
-opaque cell() :: #cell{}.
% A cell containing text contents and its desired display width.

-export_type([opts/0, cell/0]).

%--- API -----------------------------------------------------------------------

% @equiv format(Items, #{})
format(Items) -> format(Items, #{}).

% @doc Format a list of items as a table.
%
% Returns a nested IO-data that needs to be printed manually (e.g. with
% `io:format(Result)').
-spec format(Items :: list(term()), Opts :: opts()) -> iodata().
format(Items, Opts) when is_list(Items), is_map(Opts) ->
    Columns = columns(Opts),
    {Rows, ProcessedColumns} = process(Items, Columns, opts(Opts)),
    render(Rows, ProcessedColumns, Opts).

% @doc Returns a cell with the specified text contents and width.
%
% Should only be called from format functions.
%
% This function is useful when the text returned does not correspond to the
% resulting visual width, e.g. when returning colored terminal text using
% ANSI escape codes (that don't have a width when printed to the terminal).
-spec cell(iodata(), non_neg_integer()) -> cell().
cell(Text, Width) -> #cell{text = Text, width = Width}.

%--- Internal ------------------------------------------------------------------

-spec opts(opts()) -> map().
opts(Opts) -> maps:map(fun opt/2, Opts).

opt(header, true) ->
    #{format => fun format_default/1};
opt(header, #{format := Format} = Header) ->
    Header#{format => format_fun(Format)};
opt(header, Format) ->
    #{format => format_fun(Format)};
opt(columns, []) ->
    error(empty_columns);
opt(_K, V) ->
    V.

columns(#{columns := Columns}) -> columns(Columns, 1, #{});
columns(_Opts) -> #{total => 1, specs => #{}, dynamic => true}.

columns([], Index, Acc) ->
    #{total => Index, specs => Acc};
columns([Column | Columns], Index, Acc) when is_atom(Column) ->
    columns([#{key => Column} | Columns], Index, Acc);
columns([I | Columns], Index, Acc) when is_integer(I) ->
    columns([#{index => I} | Columns], Index, Acc);
columns([Column | Columns], Index, Acc) when is_map(Column) ->
    NewColumn = column(maps:merge(#{index => Index}, Column)),
    columns(Columns, Index + 1, Acc#{Index => NewColumn}).

column(Attrs) ->
    maps:update_with(
        format, fun format_fun/1, maps:merge(?COLUMN_DEFAULT, Attrs)
    ).

process(Rows, Columns, Opts) ->
    {ProcessedRows, NewColumns} = lists:mapfoldl(
        fun process_row/2, Columns, Rows
    ),
    {Header, AllColumns} = process_headers(NewColumns, Opts),
    {Header ++ ProcessedRows, AllColumns}.

process_headers(#{specs := Specs} = Columns, #{header := Header}) ->
    {Item, NewColumns} = maps:fold(
        fun(_, Column, Acc) -> process_header(Header, Column, Acc) end,
        {{}, Columns},
        Specs
    ),
    {[Item], NewColumns};
process_headers(Columns, _Opts) ->
    {[], Columns}.

process_header(#{format := Format}, #{index := Index} = Spec, State) ->
    NewSpec = Spec#{format := Format},
    case header_label(NewSpec) of
        "" -> State;
        <<>> -> State;
        Item -> process_column(Item, Index, NewSpec, {index, Index}, State)
    end.

header_label(#{name := Name}) -> Name;
header_label(#{key := Key}) -> Key;
header_label(#{index := I}) -> I.

process_row(Row, #{dynamic := true} = Columns) ->
    process_row_by_cells(next(iter(convert_proplist(Row))), {{}, Columns});
process_row(Row, #{specs := Specs} = Columns) ->
    process_row_by_columns(
        convert_proplist(Row), maps:next(maps:iterator(Specs)), {{}, Columns}
    ).

convert_proplist([{_, _}, {_, _} | _] = Proplist) ->
    try
        maps:from_list(Proplist)
    catch
        error:badarg -> Proplist
    end;
convert_proplist([{Key, Value}]) ->
    #{Key => Value};
convert_proplist(NotProplist) ->
    NotProplist.

process_row_by_cells(none, Result) ->
    Result;
process_row_by_cells({Item, Pos, Iter}, {Acc, Columns}) ->
    {ColumnIndex, Spec, NewColumns} =
        case find_spec(Pos, maps:get(specs, Columns)) of
            error ->
                S = ?COLUMN_DEFAULT,
                Total = maps:get(total, Columns),
                {Total, S#{index => Total}, Columns#{total := Total + 1}};
            #{index := I} = S ->
                {I, S, Columns}
        end,
    process_row_by_cells(
        next(Iter),
        process_column(Item, ColumnIndex, Spec, Pos, {Acc, NewColumns})
    ).

process_row_by_columns(_Row, none, Acc) ->
    Acc;
process_row_by_columns(Row, {ColumnIndex, Spec, Iter}, State) ->
    NewState =
        case get_value(Row, Spec) of
            {ok, Item} ->
                process_column(
                    Item, ColumnIndex, Spec, {index, ColumnIndex}, State
                );
            error ->
                State
        end,
    process_row_by_columns(Row, maps:next(Iter), NewState).

process_column(
    Item,
    ColumnIndex,
    #{index := CellIndex} = Spec0,
    Pos,
    {Acc, #{specs := Specs} = Columns}
) ->
    Cell = process_cell(Item, Spec0),
    Spec1 =
        case Pos of
            {index, _} -> Spec0;
            {key, Key} -> maps:merge(Spec0, #{key => Key, index => CellIndex})
        end,
    Max = fun(Width) -> max(Width, Cell#cell.width) end,
    Spec2 = maps:update_with(width, Max, Cell#cell.width, Spec1),
    NewAcc = set_cell(Acc, CellIndex, Cell),
    {NewAcc, Columns#{specs := Specs#{ColumnIndex => Spec2}}}.

process_cell(#cell{} = Cell, _Spec) ->
    Cell;
process_cell(Value, #{format := Format}) ->
    case Format(Value) of
        Cell when is_record(Cell, cell) ->
            Cell;
        Formatted when is_binary(Formatted); is_list(Formatted) ->
            #cell{text = Formatted, width = string:length(Formatted)};
        Else ->
            error({invalid_format_return, Else})
    end.

get_value(Item, #{index := Index}) when is_list(Item), length(Item) >= Index ->
    {ok, lists:nth(Index, Item)};
get_value(Item, _Spec) when is_list(Item) ->
    error;
get_value(Item, #{index := Index}) when
    is_tuple(Item), tuple_size(Item) >= Index
->
    {ok, element(Index, Item)};
get_value(Item, #{index := _Index}) when is_tuple(Item) ->
    error;
get_value(Item, #{key := Key}) when is_map(Item) ->
    {ok, maps:get(Key, Item)};
get_value(Item, #{index := _Index}) when is_map(Item) ->
    error.

set_cell(Row, Index, Cell) when Index =< tuple_size(Row) ->
    setelement(Index, Row, Cell);
set_cell(Row, Index, Cell) when Index == tuple_size(Row) + 1 ->
    erlang:append_element(Row, Cell);
set_cell(Row, Index, Cell) ->
    set_cell(erlang:append_element(Row, '_'), Index, Cell).

iter(Item) when is_list(Item) -> {list, 1, Item};
iter(Item) when is_tuple(Item) -> {tuple, 1, Item};
iter(Item) when is_map(Item) -> {map, 1, maps:next(maps:iterator(Item))};
iter(Item) -> {list, 1, [Item]}.

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

find_spec({index, Index}, Specs) ->
    case maps:find(Index, Specs) of
        {ok, Spec} -> Spec;
        error -> error
    end;
find_spec({key, Key}, Specs) ->
    find_spec_by_key(Key, maps:next(maps:iterator(Specs))).

find_spec_by_key(Key, {_Index, #{key := Key} = Spec, _Iter}) ->
    Spec;
find_spec_by_key(Key, {_Index, _Spec, Iter}) ->
    find_spec_by_key(Key, maps:next(Iter));
find_spec_by_key(_Key, none) ->
    error.

render(Rows, #{specs := Specs}, Opts) ->
    Sorted = [Spec || {_, Spec} <- lists:sort(maps:to_list(Specs))],
    render_rows(Rows, Sorted, Opts).

render_rows([Row], Specs, Opts) ->
    [render_row(Row, Specs, Opts), $\n];
render_rows([Row | Rows], Specs, Opts) ->
    [render_row(Row, Specs, Opts), $\n | render_rows(Rows, Specs, Opts)];
render_rows([], _Columns, _Opts) ->
    [$\n].

render_row(Row, Specs, Opts) -> render_row(Row, Specs, [], Opts).

render_row(_Row, [], _Pad, _Opts) ->
    [];
render_row(Row, [#{width := CWidth} = Spec | Specs], Pad, Opts) ->
    case get_cell(Row, Spec) of
        error ->
            render_row(Row, Specs, [padding(CWidth), spacer(Opts) | Pad], Opts);
        {ok, '_'} ->
            render_row(Row, Specs, [padding(CWidth), spacer(Opts) | Pad], Opts);
        {ok, Cell} ->
            {Text, Post} = align(Cell, Spec),
            [Pad, Text, render_row(Row, Specs, [Post, spacer(Opts)], Opts)]
    end.

get_cell(Row, #{index := Index}) ->
    try
        {ok, element(Index, Row)}
    catch
        error:badarg -> error
    end.

align(#cell{text = Text, width = Width}, #{width := CWidth, align := left}) ->
    {Text, padding(CWidth - Width)};
align(#cell{text = Text, width = Width}, #{width := CWidth, align := right}) ->
    {[padding(CWidth - Width), Text], []};
align(#cell{text = Text, width = Width}, #{width := CWidth, align := center}) ->
    Pad = CWidth - Width,
    Left = Pad div 2,
    {[padding(Left), Text], padding(Pad - Left)}.

padding(Width) -> lists:duplicate(Width, $\s).

format_default(Term) when is_binary(Term) ->
    Term;
format_default(Term) when is_list(Term) ->
    case io_lib:printable_list(Term) of
        true -> Term;
        false -> io_lib:format("~p", [Term])
    end;
format_default(Term) when is_atom(Term) ->
    atom_to_binary(Term, utf8);
format_default(Term) ->
    io_lib:format("~p", [Term]).

spacer(#{spacer := Spacer}) -> Spacer;
spacer(_Opts) -> <<"  ">>.

format_fun(uppercase) ->
    fun(Value) -> string:uppercase(words(format_default(Value))) end;
format_fun(titlecase) ->
    fun(Value) -> textual:titlecase(words(format_default(Value))) end;
format_fun(lowercase) ->
    fun(Value) -> string:lowercase(words(format_default(Value))) end;
format_fun(Fun) when is_function(Fun, 1) ->
    Fun;
format_fun(Format) ->
    error({invalid_format, Format}).

words(Binary) -> string:replace(Binary, <<"_">>, <<" ">>, all).
