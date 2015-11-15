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
:- use_module(library(dcg/dcg_arrow)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- meta_predicate(dcg_table(+,:,?,?)).
:- meta_predicate(dcg_table_caption(+,+,0,?,?)).
:- meta_predicate(dcg_table_cell(+,+,1,+,?,?)).
:- meta_predicate(dcg_table_data_rows(+,+,+,+,1,+,?,?)).
:- meta_predicate(dcg_table_header_row(+,+,+,+,1,-,?,?)).
:- meta_predicate(dcg_table_row(+,+,1,+,?,?)).

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

dcg_table(Rows0, Opts0) -->
  {
    meta_options(is_meta, Opts0, Opts),
    option(caption(Caption_0), Opts, _),
    option(cell(Cell_1), Opts, pl_term),
    option(indexed(Indexed), Opts, false),
    option(maximum_number_of_rows(Max), Opts, inf),
    determine_row_length(Rows0, L),
    determine_cell_width(Cell_1, Rows0, W)
  },
  dcg_table_caption(L, W, Caption_0),
  dcg_table_line(L, W),
  dcg_table_header_row(Indexed, L, W, Rows0, Cell_1, Rows),
  dcg_table_data_rows(Indexed, 1, Max, W, Cell_1, Rows),
  dcg_table_line(L, W).



%! dcg_table_caption(+Length:nonneg, +Width:nonneg :Caption_0)// is det.
% Generates the table caption.

dcg_table_caption(_, _, _:Caption_0) -->
  {var(Caption_0)}, !,
  "".
dcg_table_caption(L, W, Caption_0) -->
  {
    dcg_width(Caption_0, CW),
    N0 is floor((L * (W + 3) + 3 - CW) / 2),
    N is max(0, N0)
  },
  indent_nl(N, Caption_0).



%! dcg_table_cell(
%!   +Type:oneof(["data","header"]),
%!   +Width:nonneg,
%!   :Cell_1,
%!   +Element
%! )// is det.
% Generated an the content for a table cell (both header and data).

dcg_table_cell("data", W, Cell_1, Element) --> !,
  {
    dcg_with_output_to(codes(Cs), dcg_call_cp(Cell_1, Element)),
    length(Cs, W1), W2 is W - W1
  },
  Cs,
  indent(W2).
dcg_table_cell("header", W, Cell_1, Element) -->
  {
    dcg_with_output_to(
      codes(Cs),
      dcg_between("*", dcg_table_cell("data", Cell_1, Element))
    ),
    length(Cs, W1), W2 is W - W1
  },
  Cs,
  indent(W2).



%! dcg_table_cell_border// is det.

dcg_table_cell_border --> " | ".



%! dcg_table_data_rows(
%!   +Indexed:boolean,
%!   +Counter:positive_integer,
%!   +MaximumNumberOfRows:or([positive_integer,oneof([inf])]),
%!   +Width:nonneg,
%!   :Cell_1,
%!   +Rows:list(list)
%! )// is det.

dcg_table_data_rows(Indexed, M1, N, W, Cell_1, [H|T]) -->
  {M1 \== N}, !,
  {(Indexed == true -> Row = [M1|H] ; Row = H)},
  dcg_table_row("data", W, Cell_1, Row),
  {succ(M1, M2)},
  dcg_table_data_rows(Indexed, M2, N, W, Cell_1, T).
dcg_table_data_rows(_, _, _, _, _, _) --> "".



%! dcg_table_header_row(
%!   +Indexed:boolean,
%!   +Length:nonneg,
%!   +Width:nonneg,
%!   +Rows:list(compound),
%!   :Cell_1,
%!   -DataRows:list(compound)
%! )// is det.

dcg_table_header_row(Indexed, L, W, [head(Row0)|Rows], Cell_1, Rows) --> !,
  % If the indexed option is set, then include a first header cell
  % indicating the index number column.
  {(Indexed == true ->  Row = ["#"|Row0] ; Row = Row0)},
  dcg_table_row("header", W, Cell_1, Row),
  dcg_table_line(L, W).
dcg_table_header_row(_, _, _, Rows, _, Rows) --> "".



%! dcg_table_line(+Length:nonneg, +Width:nonneg)// is det.

dcg_table_line(L, W) -->
  {N is L * (W + 3) + 3},
  " |",
  (   {N =< 2}
  ->  horizontal_line(N)
  ;   dcg_table_line_segments(L, W)
  ),
  nl.


dcg_table_line_segments(0, _) --> !, "".
dcg_table_line_segments(L1, W) -->
  "-",
  horizontal_line(W),
  "-|",
  {succ(L2, L1)},
  dcg_table_line_segments(L2, W).



%! dcg_table_row(
%!   +Type:oneof(["data","header"]),
%!   +Width:nonneg,
%!   :Cell_1,
%!   +Row:list
%! )// is det.

dcg_table_row(Type, W, Cell_1, [H|T]) --> !,
  dcg_table_cell_border,
  dcg_table_cell(Type, W, Cell_1, H),
  dcg_table_row(Type, W, Cell_1, T).
dcg_table_row(_, _, _, []) -->
  dcg_table_cell_border, nl.





% HELPERS %

%! determine_cell_width(:Cell_1, +Rows:list(compound), -Max:nonneg) is det.

determine_cell_width(Cell_1, [head(Row)|Rows], Max):- !,
  determine_element_width(Cell_1, Row, Max0),
  Max1 is Max0 + 2,
  determine_cell_width(Cell_1, Rows, Max2),
  Max is max(Max1, Max2).
determine_cell_width(Cell_1, Ls, Max):-
  append(Ls, L),
  maplist(determine_element_width(Cell_1), L, Maxs),
  max_list(Maxs, Max).



%! determine_element_width(:Cell_1, +Element, -Width:nonneg) is det.

determine_element_width(Cell_1, Element, W):-
  dcg_width(dcg_call_cp(Cell_1, Element), W).



%! determine_row_length(+Rows:list(compound), -Length:nonneg) is det.

determine_row_length([head(Row)|_], N):- !,
  determine_row_length([Row], N).
determine_row_length([Row|_], N):-
  length(Row, N).
