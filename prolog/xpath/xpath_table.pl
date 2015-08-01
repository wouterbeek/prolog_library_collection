:- module(
  xpath_table,
  [
    xpath_table/3 % +Dom:list(compound)
                  % -Header:list
                  % -Rows:list(list)
  ]
).

/** <module> XPath table

Extracts data rows from an HTML table using XPath.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(xpath)).





%! xpath_table(+Dom:list(compound), -Header:list, -Data:list(list)) is det.

xpath_table(Table, Header, Data):-
  ignore(xpath_table_row(header, Table, Header)),
  findall(
    Data0,
    xpath_table_row(data, Table, Data0),
    Data
  ).

%! xpath_table_row(
%!   +Mode:oneof([data,header]),
%!   +Dom:list(compound),
%!   -Data:list
%! ) is det.

xpath_table_row(data, Dom, Row):- !,
  xpath(Dom, //tr, Tr),
  findall(
    Cell,
    xpath(Tr, //td(normalize_space), Cell),
    Row
  ),
  Row \== [].
xpath_table_row(header, Dom, Row):-
  xpath(Dom, //tr, Tr),
  findall(
    Cell,
    xpath(Tr, //th(normalize_space), Cell),
    Row
  ),
  Row \== [].
