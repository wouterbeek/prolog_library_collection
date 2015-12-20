:- module(
  row_ext,
  [
    row_list/2 % ?Row:compound
               % ?List:list
  ]
).

/** <module> Row extensions

@author Wouter Beek
@version 2015/12
*/





%! row_list(+Row:compound, +List:list) is semidet.
%! row_list(-Row:compound, +List:list) is det.
%! row_list(+Row:compound, -List:list) is det.

row_list(Row, List):-
  Row =.. [row|List].
