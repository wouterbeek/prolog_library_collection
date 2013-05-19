:- module(
  db_ext,
  [
    db_add/1, % +New
    db_add_novel/1, % +New
    db_replace/2, % +Old
                  % -New
    db_replace_novel/2 % +Old
                       % -New
  ]
).

/** <module> DB_EXT

Database extensions.

@author Wouter Beek
@version 2013/04-2013/05
*/

:- meta_predicate(db_add_novel(:)).
:- meta_predicate(db_replace_novel(0,0)).


db_add(New):-
  assert(New).

%! db_add_novel(+New) is det.
% Asserts the given fact only if it does not already exist.

db_add_novel(New):-
  call(New),
  !.
db_add_novel(New):-
  assert(New).

%! db_replace(+Old, +New) is det.
% Replaces exactly one asserted fact with another.

db_replace(Old, New):-
  retract(Old),
  assert(New).

db_replace_novel(Old, New):-
  call(New),
  \+ call(Old),
  !.
db_replace_novel(Old, New):-
  db_replace(Old, New).
