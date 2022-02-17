-module(grid).

% API
-export([format/1]).
-ignore_xref({format, 1}).

%--- API -----------------------------------------------------------------------

format(Items) ->
    {Rows, Columns} = render(Items),
    format(Rows, Columns, #{spacer => <<"  ">>}).

%--- Internal ------------------------------------------------------------------

render(Items) -> render(Items, {[], #{}}).

render([], {Items, Columns}) ->
    {lists:reverse(Items), [C || {_P, C} <- lists:sort(maps:to_list(Columns))]};
render([Item | Rest], {Items, Columns}) ->
    {Formatted, NewColumns} = render_item(Item, 1, {[], Columns}),
    render(Rest, {[Formatted | Items], NewColumns}).

render_item([], _Pos, {Item, Columns}) ->
    {lists:reverse(Item), Columns};
render_item([Cell | Rest], Pos, {Item, Columns}) ->
    Formatted = format_cell(Cell),
    Width = string:length(Formatted),
    NewColumns = maps:update_with(
        Pos,
        fun(#{width := OldWidth} = Column) ->
            Column#{width := max(Width, OldWidth)}
        end,
        #{width => Width},
        Columns
    ),
    render_item(Rest, Pos + 1, {[Formatted | Item], NewColumns}).

format(Items, Columns, Opts) ->
    lists:map(fun(Item) -> format_row(Item, Columns, Opts) end, Items).

format_row([], [], _Opts) ->
    [$\n];
format_row([Cell | Row], [#{width := Width} | Columns], Opts) ->
    #{spacer := Spacer} = Opts,
    Formatted = io_lib:format("~*.. s", [-Width, Cell]),
    [[Formatted | [Spacer || Row =/= []]] | format_row(Row, Columns, Opts)].

format_cell(Term) when is_binary(Term); is_list(Term) ->
    Term;
format_cell(Term) when is_integer(Term) ->
    integer_to_binary(Term);
format_cell(Term) when is_atom(Term) ->
    atom_to_binary(Term).
