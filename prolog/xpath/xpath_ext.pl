:- module(
  xpath_ext,
  [
    xpath_table/2 % +Dom, -Rows
  ]
).
:- reexport(library(xpath)).

/** <module> HTML tables

Extract data from HTML tables.

@author Wouter Beek
@version 2016/05
*/





%! xpath_table(+Dom, -Rows) is det.

xpath_table(Dom, Rows) :-
  xpath(Dom, //table, Table),
  (   xpath_table_row(header, Table, HeaderRow)
  ->  Rows = [head(HeaderRow)|DataRows]
  ;   Rows = DataRows
  ),
  findall(DataRow, xpath_table_row(data, Table, DataRow), DataRows).

xpath_table_row(Mode, Dom, Row) :-
  xpath(Dom, //tr, Tr),
  findall(Cell, xpath_row_cell(Mode, Tr, Cell), Row),
  Row \== [].

xpath_row_cell(data, Tr, Cell) :-
  xpath(Tr, //td(normalize_space), Cell).
xpath_row_cell(header, Tr, Cell) :-
  xpath(Tr, //th(normalize_space), Cell).
