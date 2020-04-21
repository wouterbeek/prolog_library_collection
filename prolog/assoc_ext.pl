:- module(
  assoc_ext,
  [
    merge_assoc/3,    % +New, +Old, -Merge
    transpose_assoc/2 % +Original, -Transposed
  ]
).
:- reexport(library(assoc)).

/** <module> Extended support for association lists

This module extends the support for association lists in the
SWI-Prolog standard library.

*/

:- use_module(library(pair_ext)).





%! merge_assoc(+New:assoc, +Old:assoc, -Merge:assoc) is det.
%
% Merges two association lists (`New` and `Old`) into one new
% association list (`Merge`).
%
% If the same key appear in both `New` and `Old`, then the value from
% `New` is used and the value from `Old` is discarded.

merge_assoc(New, Old, Merge) :-
  assoc_to_list(New, NewPairs),
  assoc_to_list(Old, OldPairs),
  merge_pairs(NewPairs, OldPairs, MergePairs),
  list_to_assoc(MergePairs, Merge).



%! transpose_assoc(+Original:assoc, -Transposed:assoc) is det.
%
% Transposes an association list, i.e., turns all keys into values and
% all values into keys.

transpose_assoc(Assoc1, Assoc2) :-
  assoc_to_list(Assoc1, Pairs1),
  transpose_pairs(Pairs1, Pairs2),
  list_to_assoc(Pairs2, Assoc2).
