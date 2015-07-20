:- module(
  html_table,
  [
    html_table/3 % +Dom:list(compound)
                 % -Header:list
                 % -Rows:list(list)
  ]
).

/** <module> HTML table

Extracts data rows from an HTML table.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(xpath)).





%! html_table(+Dom:list(compound), -Header:list, -Data:list(list)) is det.

html_table(Table, Header, Data):-
  ignore(html_table_row(header, Table, Header)),
  findall(
    Data0,
    html_table_row(data, Table, Data0),
    Data
  ).

%! html_table_row(
%!   +Mode:oneof([data,header]),
%!   +Dom:list(compound),
%!   -Data:list
%! ) is det.

html_table_row(data, Dom, Row):- !,
  xpath(Dom, //tr, Tr),
  findall(
    Cell,
    xpath(Tr, //td(normalize_space), Cell),
    Row
  ),
  Row \== [].
html_table_row(header, Dom, Row):-
  xpath(Dom, //tr, Tr),
  findall(
    Cell,
    xpath(Tr, //th(normalize_space), Cell),
    Row
  ),
  Row \== [].
