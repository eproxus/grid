% @doc Formatting library for tabular data.
-module(grid).

% API
-export([format/1]).
-ignore_xref({format, 1}).
-export([format/2]).
-ignore_xref({format, 2}).
-export([cell/2]).
-ignore_xref({cell, 2}).

-export([parse_style/1]).
-export([style/1]).

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
format(Items, UserOpts) when is_list(Items), is_map(UserOpts) ->
    Opts = opts(UserOpts),
    Columns = columns(Opts),
    {Headers, Rows, ProcessedColumns} = process(Items, Columns, Opts),
    render(Headers, Rows, ProcessedColumns, Opts).

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
opts(Opts) ->
    Default = #{style => default},
    maps:map(fun opt/2, maps:merge(Default, Opts)).

opt(header, true) ->
    #{format => fun format_default/1};
opt(header, #{format := Format} = Header) ->
    Header#{format => format_fun(Format)};
opt(header, Format) ->
    #{format => format_fun(Format)};
opt(columns, []) ->
    error(empty_columns);
opt(style, Style) ->
    style(Style);
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
    {Headers, AllColumns} = process_headers(NewColumns, Opts),
    {Headers, ProcessedRows, AllColumns}.

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

render(Headers, Rows, #{specs := Specs}, Opts) ->
    Sorted = [Spec || {_, Spec} <- lists:sort(maps:to_list(Specs))],
    render_table(Headers, Rows, Sorted, Opts).

render_table([], Rows, Specs, Opts) ->
    [
        render_border(Specs, delimiters(body_top, Opts), Opts),
        render_rows(Rows, Specs, row_delimiters(Opts), Opts)
    ];
render_table(Headers, Rows, Specs, Opts) ->
    [
        render_headers(Headers, Rows, Specs, header_delimiters(Opts), Opts),
        render_rows(Rows, Specs, row_delimiters(Opts), Opts)
    ].

header_delimiters(#{style := #{hrl := HRL, hrc := HRC, hrr := HRR}}) ->
    {HRL, HRC, HRR}.

row_delimiters(#{style := #{brl := BRL, brc := BRC, brr := BRR}}) ->
    {BRL, BRC, BRR}.

render_headers([Header], Rows, Specs, Delims, Opts) ->
    [
        render_border(Specs, delimiters(header_top, Opts), Opts),
        render_row(Header, Specs, Delims, Opts),
        $\n,
        case Rows of
            [] -> render_border(Specs, delimiters(header_bottom, Opts), Opts);
            _ -> render_border(Specs, delimiters(header_separator, Opts), Opts)
        end
    ].

delimiters(header_top, #{style := #{htl := HTL, hth := HTH, htc := HTC, htr := HTR}}) ->
    {HTL, HTH, HTC, HTR};
delimiters(header_bottom, #{style := #{hbl := HBL, hbh := HBH, hbc := HBC, hbr := HBR}}) ->
    {HBL, HBH, HBC, HBR};
delimiters(header_separator, #{style := #{hsl := HSL, hsh := HSH, hsc := HSC, hsr := HSR}}) ->
    {HSL, HSH, HSC, HSR};
delimiters(body_top, #{style := #{btl := BTL, bth := BTH, btc := BTC, btr := BTR}}) ->
    {BTL, BTH, BTC, BTR};
delimiters(body_bottom, #{style := #{bbl := BBL, bbh := BBH, bbc := BBC, bbr := BBR}}) ->
    {BBL, BBH, BBC, BBR};
delimiters(body_divider, #{style := #{bdl := BDL, bdh := BDH, bdc := BDC, bdr := BDR}}) ->
    {BDL, BDH, BDC, BDR}.

render_border(_Specs, {[], [], [], []}, _Opts) ->
    [];
render_border(Specs, {L, S, M, R}, Opts) ->
    Row = list_to_tuple([{cell, lists:duplicate(W, S), W} || #{width := W} <- Specs]),
    [render_row(Row, Specs, {L, M, R}, Opts), $\n].

render_rows([], _Specs, _Delims, _Opts) ->
    [];
render_rows([Row], Specs, Delims, Opts) ->
    [
        render_row(Row, Specs, Delims, Opts),
        $\n,
        render_border(Specs, delimiters(body_bottom, Opts), Opts)
    ];
render_rows([Row | Rows], Specs, Delims, Opts) ->
    [
        render_row(Row, Specs, Delims, Opts),
        $\n,
        render_border(Specs, delimiters(body_divider, Opts), Opts)
        | render_rows(Rows, Specs, Delims, Opts)
    ].

render_row(Row, Specs, {Left, _, _} = Delims, Opts) ->
    [Left, render_row(Row, Specs, Delims, [], Opts)].

render_row(_Row, [], {_, _, []}, _Pad, _Opts) ->
    [];
render_row(Row, [Spec], {_, _, []}, Pad, _Opts) ->
    Cell = get_cell(Row, Spec),
    {Text, _Post} = align(Cell, Spec),
    [Pad, Text];
render_row(Row, [Spec], {_, _, Right}, Pad, _Opts) ->
    Cell = get_cell(Row, Spec),
    {Text, Post} = align(Cell, Spec),
    [Pad, Text, Post, Right];
render_row(Row, [#{width := _CWidth} = Spec | Specs], {_, Middle, _} = Delims, Pad, Opts) ->
    Cell = get_cell(Row, Spec),
    {Text, Post} = align(Cell, Spec),
    [Pad, Text, render_row(Row, Specs, Delims, [Post, Middle], Opts)].

get_cell(Row, #{index := Index}) ->
    Empty = #cell{text = "", width = 0},
    try element(Index, Row) of
        '_' -> Empty;
        Text -> Text
    catch
        error:badarg -> Empty
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

% FIXME: Retain support for old `spacer` option?
% spacer(#{style := #{brc := Spacer}}) -> Spacer;
% spacer(#{spacer := Spacer}) -> Spacer;
% spacer(_Opts) -> <<"  ">>.

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

% Format:
% ```
%   1234
% 1 ┏━┳┓ header top
% 2 ┣━╋┫ header divider
% 3 ┃ ┃┃ header row
% 4 ┗━┻┛ header bottom
% 5 ┡━╇┩ header body divider
% 6 ┌─┬┐ body top
% 7 ├─┼┤ body divider
% 8 │ ││ body row
% 9 └─┴┘ body bottom
% ```
% * Rows 6-9 can be ommitted to use the same style for header and body.
% * Additionally, row 5 can be ommitted to use the same divider style for both.
% * Optionally columns 1 and 4 can be ommitted for a borderless style.
%
% Header characters:
% ```
% ┏(htl) ━(hth) ┳(htc) ┓(htr)    // header top left/horizontal/cross/right
% ┣(hdl) ━(hdh) ╋(hdc) ┫(hdr)    // header divider left/horizontal/cross/right
% ┃(hrl) space  ┃(hrc) ┃(hrr)    // header row left/space/cross/right
% ┗(hbl) ━(hbh) ┻(hbc) ┛(hbr)    // header bottom left/horizontal/cross/right
% ┡(hsl) ━(hsh) ╇(hsc) ┩(hsr)    // header-body separator left/horizontal/cross/right
% ```
%
% Body characters:
% ```
% ┌(btl) ─(bth) ┬(btc) ┐(btr)    // body top left/horizontal/cross/right
% ├(bdl) ─(bdh) ┼(bdc) ┤(bdr)    // body divider left/horizontal/cross/right
% │(brl) space  │(brc) │(brr)    // body row left/space/cross/right
% └(bbl) ─(bbh) ┴(bbc) ┘(bbr)    // body bottom left/horizontal/cross/right
% ```
% erlfmt-ignore
parse_style(Def) ->
    Empty = {[], [], [], []},
    Rows = string:split(Def, <<"\n">>, all),
    Sections = case [parse_style_row(string:to_graphemes(R)) || R <- Rows] of
        % Bordered / Unique header, separator and body styles (9x4)
        [HT, HD, HR, HB, HS, BT, BD, BR, BB] ->
            [HT, HD, HR, HB, HS, BT, BD, BR, BB];
        % Bordered / Same style for header and body, with border (5x4)
        [HT, HD, HR, HB, HS] ->
            [HT, HD, HR, HB, HS, HT, HD, HR, HB];
        % Bordered / Same style for header, body and separator (4x4)
        [HT, HD, HR, HB] ->
            [HT, HD, HR, HB, HD, HT, HD, HR, HB];
        % Borderless / Unique header, separator and body styles (6x2)
        [{"", _, _, ""} = HD, HR, HS, BD, BR] ->
            [Empty, HD, HR, Empty, HS, Empty, BD, BR, Empty];
        % Borderless / Same style for header, body and separator (2x2)
        [{"", _, _, ""} = HD, HR] ->
            [Empty, HD, HR, Empty, HD, Empty, HD, HR, Empty]
    end,
    [
        {HTL, HTH, HTC, HTR},
        {HDL, HDH, HDC, HDR},
        {HRL, _,   HRC, HRR},
        {HBL, HBH, HBC, HBR},
        {HSL, HSH, HSC, HSR},
        {BTL, BTH, BTC, BTR},
        {BDL, BDH, BDC, BDR},
        {BRL, _,   BRC, BRR},
        {BBL, BBH, BBC, BBR}
    ] = Sections,
    #{
        htl => HTL, hth => HTH, htc => HTC, htr => HTR,
        hdl => HDL, hdh => HDH, hdc => HDC, hdr => HDR,
        hrl => HRL, hrc => HRC, hrr => HRR,
        hbl => HBL, hbh => HBH, hbc => HBC, hbr => HBR,
        hsl => HSL, hsh => HSH, hsc => HSC, hsr => HSR,
        btl => BTL, bth => BTH, btc => BTC, btr => BTR,
        bdl => BDL, bdh => BDH, bdc => BDC, bdr => BDR,
        brl => BRL, brc => BRC, brr => BRR,
        bbl => BBL, bbh => BBH, bbc => BBC, bbr => BBR
    }.

parse_style_row([A, B, C, D]) -> {A, B, C, D};
parse_style_row([B, C]) -> {"", B, C, ""}.

% erlfmt-ignore
style(default) ->
    #{bbl => [],   htc => [],   bth => [],   hbh => [],   hsc => [],
      hdl => [],   hth => [],   bbh => [],   btc => [],   btr => [],
      brc => "  ", hsr => [],   bdr => [],   htr => [],   bdh => [],
      hbl => [],   hbc => [],   bbc => [],   bdc => [],   hbr => [],
      brl => [],   hdr => [],   htl => [],   hrr => [],   hrc => "  ",
      brr => [],   bbr => [],   hdc => [],   hrl => [],   hsh => [],
      hsl => [],   hdh => [],   bdl => [],   btl => []};
style(simple) ->
    % rp(grid:parse_style(~"""
    % ┌─┬┐
    % ├─┼┤
    % │ ││
    % └─┴┘
    % """)).
    #{bbl => 9492, htc => 9516, bth => 9472, hbh => 9472, hsc => 9532,
      hdl => 9500, hth => 9472, bbh => 9472, btc => 9516, btr => 9488,
      brc => 9474, hsr => 9508, bdr => 9508, htr => 9488, bdh => 9472,
      hbl => 9492, hbc => 9524, bbc => 9524, bdc => 9532, hbr => 9496,
      brl => 9474, hdr => 9508, htl => 9484, hrr => 9474, hrc => 9474,
      brr => 9474, bbr => 9496, hdc => 9532, hrl => 9474, hsh => 9472,
      hsl => 9500, hdh => 9472, bdl => 9500, btl => 9484};
style(borderless) ->
    % FIXME: Buggy, ╇ get's used as brc when it should be │
    % rp(grid:parse_style(~"""
    % ━╋
    %  ┃
    % ━╇
    % ─┼
    %  │
    % """)).
    #{bbl => [],   htc => 9547, bth => 9473, hbh => 9472, hsc => 9474,
      hdl => [],   hth => 9473, bbh => 9472, btc => 9547, btr => [],
      brc => 9543, hsr => [],   bdr => [],   htr => [],   bdh => 32,
      hbl => [],   hbc => 9532, bbc => 9532, bdc => 9475, hbr => [],
      brl => [],   hdr => [],   htl => [],   hrr => [],   hrc => 9543,
      brr => [],   bbr => [],   hdc => 9475, hrl => [],   hsh => 32,
      hsl => [],   hdh => 32,   bdl => [],   btl => []};
style(simple_borderless) ->
    % rp(grid:parse_style(~"""
    % ─┼
    %  │
    % """)).
    #{bbl => [],   htc => [],   bth => [],   hbh => [],   hsc => 9532,
      hdl => [],   hth => [],   bbh => [],   btc => [],   btr => [],
      brc => 9474, hsr => [],   bdr => [],   htr => [],   bdh => 9472,
      hbl => [],   hbc => [],   bbc => [],   bdc => 9532, hbr => [],
      brl => [],   hdr => [],   htl => [],   hrr => [],   hrc => 9474,
      brr => [],   bbr => [],   hdc => 9532, hrl => [],   hsh => 9472,
      hsl => [],   hdh => 9472, bdl => [],   btl => []};
style(square) ->
    % FIXME: Buggy, doesn't use header row styles
    % rp(grid:parse_style(~"""
    % ┏━┳┓
    % ┣━╋┫
    % ┃ ┃┃
    % ┗━┻┛
    % ┡━╇┩
    % ┌─┬┐
    % ├─┼┤
    % │ ││
    % └─┴┘
    % """));
    #{bbl => 9492, htc => 9523, bth => 9472, hbh => 9473, hsc => 9543,
      hdl => 9507, hth => 9473, bbh => 9472, btc => 9516, btr => 9488,
      brc => 9474, hsr => 9513, bdr => 9508, htr => 9491, bdh => 9472,
      hbl => 9495, hbc => 9531, bbc => 9524, bdc => 9532, hbr => 9499,
      brl => 9474, hdr => 9515, htl => 9487, hrr => 9475, hrc => 9475,
      brr => 9474, bbr => 9496, hdc => 9547, hrl => 9475, hsh => 9473,
      hsl => 9505, hdh => 9473, bdl => 9500, btl => 9484};
style(rounded) ->
    % rp(grid:parse_style(~"""
    % ╭─┬╮
    % ├╌┼┤
    % │ ││
    % ╰─┴╯
    % ╞═╪╡
    % """));
    #{brr => 9474,bth => 9472,htr => 9582,hsh => 9552,brc => 9474,
      bbh => 9472,hbc => 9524,bdh => 9548,hrl => 9474,hsl => 9566,
      brl => 9474,btc => 9516,bdc => 9532,htc => 9516,hdr => 9508,
      bbr => 9583,hbr => 9583,hsc => 9578,btr => 9582,bbc => 9524,
      hdh => 9548,btl => 9581,hrr => 9474,bdr => 9508,htl => 9581,
      hsr => 9569,hdc => 9532,bdl => 9500,bbl => 9584,hbl => 9584,
      hrc => 9474,hth => 9472,hbh => 9472,hdl => 9500};
style(ascii) ->
    % rp(grid:parse_style(~"""
    % +-++
    % +-++
    % | ||
    % +-++
    % """));
    #{bbl => 43,htc => 43,bth => 45,hbh => 45,hsc => 43,hdl => 43,
      hth => 45,bbh => 45,btc => 43,btr => 43,brc => 124,
      hsr => 43,bdr => 43,htr => 43,bdh => 45,hbl => 43,hbc => 43,
      bbc => 43,bdc => 43,hbr => 43,brl => 124,hdr => 43,
      htl => 43,hrr => 124,hrc => 124,brr => 124,bbr => 43,
      hdc => 43,hrl => 124,hsh => 45,hsl => 43,hdh => 45,
      bdl => 43,btl => 43};
style(ascii_borderless) ->
    % rp(grid:parse_style(~"""
    % -+
    %  |
    % """));
    #{bbl => [],htc => [],bth => [],hbh => [],hsc => 43,hdl => [],
      hth => [],bbh => [],btc => [],btr => [],brc => 124,
      hsr => [],bdr => [],htr => [],bdh => 45,hbl => [],hbc => [],
      bbc => [],bdc => 43,hbr => [],brl => [],hdr => [],htl => [],
      hrr => [],hrc => 124,brr => [],bbr => [],hdc => 43,
      hrl => [],hsh => 45,hsl => [],hdh => 45,bdl => [],btl => []};
style(Style) ->
    error({invalid_style, Style}).
