:- module(
  dcg_table,
  [
    dcg_table//1, % +Rows
    dcg_table//2  % +Rows, :Opts
  ]
).

/** <module> DCG Tables

Generates tables for text-based display.

# State

```pl
state(CurrentRow, LastRow, ColWidths)
```

@author Wouter Beek
@version 2015/11-2016/01, 2016/08, 2017/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_cli)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(default)).
:- use_module(library(list_ext)).
:- use_module(library(math/math_ext)).
:- use_module(library(option)).
:- use_module(library(string_ext)).

:- meta_predicate
    column_widths(1, +, -),
    dcg_table_cell(+, +, 1, +, ?, ?),
    dcg_table_data_rows(+, 1, +, -, ?, ?),
    dcg_table_line(+, //, ?, ?),
    dcg_table_row(+, +, 1, +, ?, ?),
    table_state(1, +, -).

is_meta(cell).

:- multifile
    dcg:dcg_hook//1.





% API %

%! dcg_table(+Rows)// is det.
%! dcg_table(+Rows, :Opts)// is det.
%
% The following options are supported:
%
%   * cell(+callable)
%
%   * indexed(+boolean)
%
%   * maximum_number_of_rows(+nonneg)

dcg_table(L) -->
  dcg_table(L, []).


dcg_table(Rows1, Opts0) -->
  {
    meta_options(is_meta, Opts0, Opts),
    option(cell(Cell_1), Opts, dcg:dcg_hook),
    prepare_table(Rows1, Rows2, Opts),
    table_state(Cell_1, Rows2, State1)
  },
  % TOP: HEADER ROW + FIRST LINES
  (   {Rows2 = [head(Row)|Rows3]}
  ->  dcg_table_top_line(State1),
      {State1 = state(_,_,Ws)},
      dcg_table_row(head, Ws, Cell_1, Row),
      (   {State1 = state(_,0,_)}
      ->  dcg_table_bot_line(State1)
      ;   dcg_table_mid_line(State1)
      )
  ;   {State1 = state(_,0,_)}
  ->  ""
  ;   dcg_table_top_line(State1)
  ),
  % REST: DATA ROWS + BOTTOM LINE
  (   {State1 = state(_,0,_)}
  ->  ""
  ;   dcg_table_data_rows(State1, Cell_1, Rows3, State2),
      dcg_table_bot_line(State2)
  ).



%! dcg_table_cell(+Type, +ContentWidth, :Cell_1, +Elem)// is det.
%
% Type is either `head` or `data`.

dcg_table_cell(Type, MaxContentWidth, Cell_1, Elem) --> !,
  {
    string_phrase(dcg_call_cp(Cell_1, Elem), Content),
    string_truncate(Content, MaxContentWidth, TruncatedContent),
    (   Content == TruncatedContent
    ->  string_length(Content, ContentWidth),
        Indent is MaxContentWidth - ContentWidth
    ;   Indent = 0
    )
  },
  " ",
  ({Type == data} -> str(Content) ; bold(Content)),
  " ",
  indent(Indent).



%! dcg_table_data_rows(+State1, :Cell_1, +Rows, -State2)// is det.

dcg_table_data_rows(State, _, [], State) --> !, "".
dcg_table_data_rows(State1, Cell_1, [H|T], State3) -->
  {State1 = state(_,_,Ws)},
  dcg_table_row(data, Ws, Cell_1, H),
  {next_row(State1, State2)},
  dcg_table_data_rows(State2, Cell_1, T, State3).



%! dcg_table_row(+Type, +Ws, :Cell_1, +Row)// is det.

dcg_table_row(Type, [W|Ws], Cell_1, [H|T]) --> !,
  "│",
  dcg_table_cell(Type, W, Cell_1, H),
  dcg_table_row(Type, Ws, Cell_1, T).
dcg_table_row(_, _, _, []) -->
  "│",
  nl.




% LINE %

%! dcg_table_bot_line(+State)// is det.

dcg_table_bot_line(State) -->
  "└",
  {State = state(_,_,Ws)},
  dcg_table_line(Ws, "┴"),
  "┘",
  nl.



%! dcg_table_mid_line(+State)// is det.

dcg_table_mid_line(State) -->
  "├",
  {State = state(_,_,Ws)},
  dcg_table_line(Ws, "┼"),
  "┤",
  nl.



%! dcg_table_top_line(+State)// is det.

dcg_table_top_line(State) -->
  "┌",
  {State = state(_,_,Ws)},
  dcg_table_line(Ws, "┬"),
  "┐",
  nl.



%! dcg_table_line(+Ws, :Sep_0)// is det.

dcg_table_line([], _) --> !, "".
dcg_table_line([H], _) --> !,
  {H0 is H + 2},
  #(H0, bar), !.
dcg_table_line([H|T], Sep_0) -->
  {H0 is H + 2},
  #(H0, bar), !,
  Sep_0,
  dcg_table_line(T, Sep_0).

bar --> "─".





% STATE %

%! column_widths(:Cell_1, +Rows, -Widths) is det.

column_widths(Cell_1, Rows, MaxWs) :-
  rows_to_cols(Rows, Cols),
  maplist(dcg_max_width(Cell_1), Cols, MaxWs).



%! next_row(+State1, -State2) is det.

next_row(state(Row1,Last,Ws),state(Row2,Last,Ws)) :-
  Row2 is Row1 + 1.



%! number_of_columns(+Rows, -NumCols) is det.

number_of_columns([], 0) :- !.
number_of_columns([H0|_], Len) :-
  row_list(H0, H),
  length(H, Len).



%! number_of_rows(+Rows, -NumRows) is det.

number_of_rows([head(_)|Rows], NumRows) :- !,
  number_of_rows(Rows, NumRows).
number_of_rows(Rows, NumRows) :-
  length(Rows, NumRows).



%! row_list(+Row, -List) is det.

row_list(head(Row), Row) :- !.
row_list(Row, Row).



%! rows_to_cols(+Rows, -Cols) is det.

rows_to_cols(Rows, Cols) :-
  number_of_columns(Rows, N),
  repeating_list([], N, Cols0),
  rows_to_cols(Rows, Cols0, Cols).

rows_to_cols([], Sol, Sol) :- !.
rows_to_cols([H10|T1], L1, Sol) :-
  row_list(H10, H1),
  maplist(add_head, H1, L1, L2),
  rows_to_cols(T1, L2, Sol).

add_head(H, T, [H|T]).



%! table_state(:Cell_1, +Rows, -State) is det.

table_state(Cell_1, Rows, state(0,NumRows,Ws)) :-
  number_of_rows(Rows, NumRows),
  column_widths(Cell_1, Rows, Ws).





% PREPARATION %

%! prepare_table(+Rows1, -Rows2, +Opts) is det.

prepare_table(Rows1, Rows3, Opts) :-
  option(indexed(Indexed), Opts, false),
  (Indexed == true -> index_table(Rows1, Rows2) ; Rows2 = Rows1),
  option(maximum_number_of_rows(Max), Opts, inf),
  max_table(Max, Rows2, Rows3).



%! max_table(+Max, +Rows1, -Rows2) is det.
%
% Restrict Rows1 to include at most Max number of rows.

max_table(inf, Rows, Rows) :- !.
max_table(Max, [head(Row)|Rows1], [head(Row)|Rows2]) :- !,
  max_table(Max, Rows1, Rows2).
max_table(Max, Rows1, Rows2) :-
  prefix(Rows2, Max, Rows1).



%! index_table(+Rows1, -Rows2) is det.
%
% Add indexes to all rows, and to the header row (if present).

index_table([head(Row)|Rows1], [head(["№"|Row])|Rows2]) :-
  index_table(1, Rows1, Rows2).

index_table(_, [], []) :- !.
index_table(N1, [Row|Rows1], [[N1|Row]|Rows2]) :-
  N2 is N1 + 1,
  index_table(N2, Rows1, Rows2).

/*
add_sum_row(Rows1, Rows2, Opts) :-
  option(sum_col(Cols0), Opts, []),
  sort(Cols0, Cols),
  number_of_columns(Rows1, Last),
  add_sum_row0(1, Last, Rows1, Cols, Row),
  append(Rows1, [Row], Rows2).

add_sum_row0(Last, Last, _, _, []) :- !.
add_sum_row0(Col1, Last, Rows, [Col1|Cols], [Sum|T]) :- !,
  maplist(nth0(Col1), Rows, Vals),
  sum_list(Vals, Sum),
  Col2 is Col1 + 1,
  add_sum_row0(Col2, Last, Rows, Cols, T).
add_sum_row0(Col1, Last, Rows, Cols, [_|T]) :-
  Col2 is Col1 + 1,
  add_sum_row0(Col2, Last, Rows, Cols, T).
*/
