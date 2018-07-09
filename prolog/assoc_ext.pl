:- module(
  assoc_ext,
  [
    merge_assoc/3,    % +New, +Old, -Merge
    transpose_assoc/2 % +Original, -Transposed
  ]
).
:- reexport(library(assoc)).

/** <module> Assoc extensions

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).

:- use_module(library(pair_ext)).





%! merge_assoc(+New:assoc, +Old:assoc, -Merge:assoc) is det.

merge_assoc(New1, Old1, Merge) :-
  maplist(assoc_to_list, [New1,Old1], [New2,Old2]),
  merge_pairs(New2, Old2, Merge).



%! transpose_assoc(+Original:assoc, -Transposed:assoc) is det.

transpose_assoc(Assoc1, Assoc2) :-
  assoc_to_list(Assoc1, Pairs1),
  transpose_pairs(Pairs1, Pairs2),
  list_to_assoc(Pairs2, Assoc2).
