:- module(
  dcg_table,
  [
    dcg_table//1, % +Rows:list(compound)
    dcg_table//2 % +Rows:list(compound)
                 % +Options:list(compound)
  ]
).

/** <module> DCG Tables

Generates tables for text-based display.

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(default)).
:- use_module(library(list_ext)).
:- use_module(library(math/math_ext)).
:- use_module(library(option)).

:- meta_predicate(column_widths(1,+,+,-)).
:- meta_predicate(column_widths0(1,+,+,-)).
:- meta_predicate(dcg_table(+,:,?,?)).
:- meta_predicate(dcg_table_caption(+,0,?,?)).
:- meta_predicate(dcg_table_cell(+,+,1,+,?,?)).
:- meta_predicate(dcg_table_data_rows(+,+,+,1,+,-,?,?)).
:- meta_predicate(dcg_table_header_row(+,+,+,1,-,-,?,?)).
:- meta_predicate(dcg_table_row(+,+,1,+,?,?)).
:- meta_predicate(dcg_table_row(+,+,+,1,+,?,?)).
:- meta_predicate(table_position(1,+,-)).

is_meta(caption).
is_meta(cell).

:- predicate_options(dcg_table//2, 2, [
     caption(+callable),
     cell(+callable),
     indexed(+boolean),
     maximum_number_of_rows(+positive_integer)
   ]).





%! dcg_table(+Rows:list(compound))// is det.
% Wrapper around dcg_table//2 with default options.

dcg_table(L) -->
  dcg_table(L, []).


%! dcg_table(+Rows:list(compound), +Options:list(compound))// is det.

dcg_table(Rows1, Opts0) -->
  {
    meta_options(is_meta, Opts0, Opts),
    option(caption(Caption_0), Opts, _),
    option(cell(Cell_1), Opts, pl_term),
    option(indexed(Ind), Opts, false),
    option(maximum_number_of_rows(Max), Opts, inf),
    %%%%add_sum_row(Rows1, Rows2, Opts),
    table_position(Cell_1, Rows1, Pos1)
  },
  dcg_table_caption(Pos1, Caption_0),
  dcg_table_line(Pos1),
  dcg_table_header_row(Ind, Pos1, Rows1, Cell_1, Pos2, Rows2),
  dcg_table_data_rows(Ind, Pos2, Max, Cell_1, Rows2, Pos3),
  dcg_table_line(Pos3).


add_sum_row(Rows1, Rows2, Opts):-
  option(sum_col(Cols0), Opts, []),
  sort(Cols0, Cols),
  number_of_columns(Rows1, Last),
  add_sum_row0(1, Last, Rows1, Cols, Row),
  append(Rows1, [Row], Rows2).

add_sum_row0(Last, Last, _, _, []):- !.
add_sum_row0(Col1, Last, Rows, [Col1|Cols], [Sum|T]):- !,
  maplist(nth0(Col1), Rows, Vals),
  sum_list(Vals, Sum),
  Col2 is Col1 + 1,
  add_sum_row0(Col2, Last, Rows, Cols, T).
add_sum_row0(Col1, Last, Rows, Cols, [_|T]):-
  Col2 is Col1 + 1,
  add_sum_row0(Col2, Last, Rows, Cols, T).



%! dcg_table_caption(+Position:compound, :Caption_0)// is det.
% Generates the table caption.

dcg_table_caption(_, Caption_0) -->
  {strip_module(Caption_0, _, BareCaption_0), var(BareCaption_0)}, !,
  "".
dcg_table_caption(pos(_,_,_,Len), Caption_0) -->
  {
    dcg_width(Caption_0, CaptionLen),
    N0 is floor((Len - CaptionLen) / 2),
    N is max(0, N0)
  },
  indent_nl(N, Caption_0).



%! dcg_table_cell(
%!   +Type:oneof(["data","header"]),
%!   +ColumnWidth:positive_integer,
%!   :Cell_1,
%!   +Element
%! )// is det.
% Generated an the content for a table cell (both header and data).

dcg_table_cell(Type, Width, Cell_1, X) --> !,
  {
    dcg_with_output_to(codes(Cs), dcg_call_cp(Cell_1, X)),
    length(Cs, CellLen),
    SpareLen0 is Width - CellLen + 1,
    SpareLen is max(0,SpareLen0)
  },
  dcg_table_cell_content(Type, Cs),
  indent(SpareLen).



%! dcg_table_cell_content(
%!   +Type:oneof(["data","header"]),
%!   +Codes:list(code)
%! )// is det.
% @tbd Use bold face for header content.

dcg_table_cell_content("data", Cs) --> !, Cs.
dcg_table_cell_content("header", Cs) --> Cs.



%! dcg_table_data_rows(
%!   +Indexed:boolean,
%!   +StartPosition:compound,
%!   +MaximumNumberOfRows:positive_integer,
%!   :Cell_1,
%!   +Rows:list(list),
%!   -EndPosition:compound
%! )// is det.

dcg_table_data_rows(Ind, Pos1, Max, Cell_1, [H|T], Pos3) -->
  {Pos1 = pos(_,row(Row1,_,_),_,_), Row1 \== Max}, !,
  {(Ind == true -> L = [Row1|H] ; L = H)},
  dcg_table_row("data", Pos1, Cell_1, L),
  {next_position_row(Pos1, Pos2)},
  dcg_table_data_rows(Ind, Pos2, Max, Cell_1, T, Pos3).
dcg_table_data_rows(_, Pos, _, _, _, Pos) --> "".



%! dcg_table_header_row(
%!   +Indexed:boolean,
%!   +StartPosition:compound,
%!   +Rows:list,
%!   :Cell_1,
%!   -EndPosition:compound,
%!   -DataRows:list
%! )// is det.

dcg_table_header_row(Indexed, Pos1, [head(Row0)|Rows], Cell_1, Pos2, Rows) --> !,
  % If the indexed option is set, then include a first header cell
  % indicating the index number column.
  {(Indexed == true ->  Row = ["#"|Row0] ; Row = Row0)},
  dcg_table_row("header", Pos1, Cell_1, Row),
  {next_position_row(Pos1, Pos2)},
  dcg_table_line(Pos2).
dcg_table_header_row(_, Pos, Rows, _, Pos, Rows) --> "".



%! dcg_table_row(
%!   +Type:oneof(["data","header"]),
%!   +Position:compound,
%!   :Cell_1,
%!   +Row:list
%! )// is det.

dcg_table_row(Type, pos(Col,Row,Ws,Len), Cell_1, L) -->
  dcg_table_row(Type, pos(Col,Row,Ws,Len), Ws, Cell_1, L).


%! dcg_table_row(
%!   +Type:oneof(["data","header"]),
%!   +Position:compound,
%!   +ColumnWidths:list(positive_integer),
%!   :Cell_1,
%!   +Row:list
%! )// is det.
dcg_table_row(Type, Pos, [W|Ws], Cell_1, [H|T]) --> !,
  cell_border,
  dcg_table_cell(Type, W, Cell_1, H),
  dcg_table_row(Type, Pos, Ws, Cell_1, T).
dcg_table_row(_, _, _, _, []) -->
  cell_border, nl.




% LINE %

%! dcg_table_line(+Position:compound)// is det.

dcg_table_line(Pos) -->
  dcg_table_start_of_line(Pos),
  dcg_table_middle_of_line(Pos),
  dcg_table_end_of_line(Pos),
  nl, !.



%! dcg_table_end_of_line(+Position:compound)// is det.

dcg_table_end_of_line(pos(_,row(0,1,_),_,_)) --> !, "┐".
dcg_table_end_of_line(pos(_,row(Last,_,Last),_,_)) --> !, "┘".
dcg_table_end_of_line(_) --> "┤".



%! dcg_table_middle_of_line(+Position:compound)// is det.

dcg_table_middle_of_line(pos(_,_,[H],_)) --> !,
  {H0 is H + 2},
  #(H0, bar, []), !.
dcg_table_middle_of_line(pos(Col,row(Row1,Row2,Last),[H|T],Len)) -->
  {H0 is H + 2},
  #(H0, bar, []), !,
  ({Row2 =:= 1} -> "┬" ; {Row1 =:= Last} -> "┴" ; "┼"),
  dcg_table_middle_of_line(pos(Col,row(Row1,Row2,Last),T,Len)).
bar --> "─".



%! dcg_table_start_of_line(+Position:compound)// is det.

dcg_table_start_of_line(pos(_,row(0,1,_),_,_)) --> !, "┌".
dcg_table_start_of_line(pos(_,row(Last,_,Last),_,_)) --> !, "└".
dcg_table_start_of_line(_) --> "├".





% HELPERS %

%! cell_border// is det.

cell_border --> "│ ".



%! column_widths(
%!   :Cell_1,
%!   +NumberOfColumns:nonneg,
%!   +Rows:list,
%!   -Widths:list(nonneg)
%! ) is det.

column_widths(Cell_1, Rows, MaxWs):-
  rows_to_cols(Rows, Cols),
  maplist(dcg_max_width(Cell_1), Cols, Maxws).



%! next_position_row(+Position:compound, -NextPosition:compound) is det.

next_position_row(
  pos(Col,row(_,Row2,Last),Ws,Len),
  pos(Col,row(Row2,Row3,Last),Ws,Len)
):-
  succ(Row2, Row3).



%! number_of_columns(+Rows:list(compound), -NumberOfColumns:nonneg) is det.

number_of_columns([H0|T], Len):-
  row_list(H0, H),
  length(H, Len).



%! number_of_rows(+Rows:list(compound), -NumberOfRows:nonneg) is det.

number_of_rows(L, Len):-
  length(L, Len).



%! row_list(+Row:compound, -List:list) is det.

row_list(head(L), L):- !.
row_list(L, L).



%! rows_to_cols(+Rows:list(compound), -Columns:list) is det.

rows_to_cols(Rows, Cols):-
  number_of_columns(Rows, N),
  repeating_list([], N, Cols0),
  rows_to_cols(Rows, Cols0, Cols).

rows_to_cols([], Sol, Sol):- !.
rows_to_cols([H10|T1], L1, Sol):-
  row_list(H10, H1),
  maplist(add_head, H1, L1, L2),
  rows_to_cols(T1, L2, Sol).

add_head(H, T, [H|T]).



%! table_position(:Cell_1, +Rows:list, -Position:compound) is det.

table_position(Cell_1, L, pos(col(0,1,Cols),row(0,1,Rows),Ws,Len)):-
  number_of_columns(L, Cols),
  number_of_rows(L, Rows),
  column_widths(Cell_1, Cols, L, Ws),
  sum_list([1,Cols,Cols,Cols|Ws], Len).
