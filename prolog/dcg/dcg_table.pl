:- module(
  dcg_table,
  [
    dcg_table//1, % +Rows:list(list)
    dcg_table//2 % +Rows:list(list)
                 % +Options:list(compound)
  ]
).

/** <module> DCG Tables

Generates tables for text-based display.

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_arrow)).
:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(option)).

:- meta_predicate(dcg_table(+,:,?,?)).
:- meta_predicate(dcg_table_caption(0,?,?)).
:- meta_predicate(dcg_table_cell(+,1,+,?,?)).
:- meta_predicate(dcg_table_data_rows(+,+,+,1,+,?,?)).
:- meta_predicate(dcg_table_header_row(+,+,1,-,?,?)).
:- meta_predicate(dcg_table_row(+,1,+,?,?)).

is_meta(caption).
is_meta(cell).

:- predicate_options(dcg_table//2, 2, [
     caption(+callable),
     cell(+callable),
     indexed(+boolean),
     maximum_number_of_rows(+positive_integer)
   ]).





%! dcg_table(+Rows:list(list))// is det.
% Wrapper around dcg_table//2 with default options.

dcg_table(L) -->
  dcg_table(L, []).


%! dcg_table(+Rows:list(list), +Options:list(compound))// is det.

dcg_table(Rows0, Opts0) -->
  {
    meta_options(is_meta, Opts0, Opts),
    option(caption(Caption_0), Opts, _),
    option(cell(Cell_1), Opts, pl_term),
    option(indexed(Indexed), Opts, false),
    option(maximum_number_of_rows(Max), Opts, inf)
  },
  dcg_table_caption(Caption_0),
  dcg_table_line,
  dcg_table_header_row(Indexed, Rows0, Cell_1, Rows),
  dcg_table_data_rows(Indexed, 1, Max, Cell_1, Rows),
  dcg_table_line.



%! dcg_table_caption(:Caption_0)// is det.
% Generates the table caption.

dcg_table_caption(_:Caption_0) --> {var(Caption_0)}, !, "".
dcg_table_caption(Caption_0) --> dcg_call_cp(Caption_0), nl.



%! dcg_table_cell(+Type:oneof(["data","header"]), :Cell_1, +Element)// is det.
% Generated an the content for a table cell (both header and data).

dcg_table_cell("data", Cell_1, Element) --> !,
  dcg_call_cp(Cell_1, Element).
dcg_table_cell("header", Cell_1, Element) -->
  dcg_between("*", dcg_table_cell("data", Cell_1, Element)).



%! dcg_table_cell_border// is det.

dcg_table_cell_border --> " | ".



%! dcg_table_data_rows(
%!   +Indexed:boolean,
%!   +Counter:positive_integer,
%!   +MaximumNumberOfRows:or([positive_integer,oneof([inf])]),
%!   :Cell_1,
%!   +Rows:list(list)
%! )// is det.

dcg_table_data_rows(Indexed, M1, N, Cell_1, [H|T]) -->
  {M1 \== N}, !,
  {(Indexed == true -> Row = [M1|H] ; Row = H)},
  dcg_table_row("data", Cell_1, Row),
  {succ(M1, M2)},
  dcg_table_data_rows(Indexed, M2, N, Cell_1, T).
dcg_table_data_rows(_, _, _, _, _) --> "".



%! dcg_table_header_row(
%!   +Indexed:boolean,
%!   +Rows:list(list),
%!   :Cell_1,
%!   -DataRows:list(list)
%! )// is det.

dcg_table_header_row(Indexed, [head(Row0)|Rows], Cell_1, Rows) --> !,
  % If the indexed option is set, then include a first header cell
  % indicating the index number column.
  {(Indexed == true ->  Row = ["#"|Row0] ; Row = Row0)},
  dcg_table_row("header", Cell_1, Row),
  dcg_table_line.
dcg_table_header_row(_, Rows, _, Rows) --> "".



%! dcg_table_line// is det.

dcg_table_line --> horizontal_line(60), nl.



%! dcg_table_row(+Type:oneof(["data","header"]), :Cell_1, +Row:list)// is det.

dcg_table_row(Type, Cell_1, [H|T]) --> !,
  dcg_table_cell_border,
  dcg_table_cell(Type, Cell_1, H),
  dcg_table_row(Type, Cell_1, T).
dcg_table_row(_, _, []) -->
  dcg_table_cell_border, nl.
