:- module(
  assoc_ext,
  [
    transpose_assoc/2 % +Assoc1, -Assoc2
  ]
).
:- reexport(library(assoc)).

/** <module> Assoc extensions

@author Wouter Beek
@version 2018
*/

:- use_module(library(pairs)).





%! transpose_assoc(+Assoc1:assoc, -Assoc2:assoc) is det.

transpose_assoc(Assoc1, Assoc2) :-
  assoc_to_list(Assoc1, Pairs1),
  transpose_pairs(Pairs1, Pairs2),
  list_to_assoc(Pairs2, Assoc2).
