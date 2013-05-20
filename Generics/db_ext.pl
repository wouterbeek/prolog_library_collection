:- module(
  db_ext,
  [
    db_add/1, % +New
    db_add_novel/1, % +New
    db_replace/1, % +New
    db_replace/2, % +Old
                  % -New
    db_replace_novel/1, % +New
    db_replace_novel/2 % +Old
                       % -New
  ]
).

/** <module> DB_EXT

Database extensions.

@author Wouter Beek
@version 2013/04-2013/05
*/

:- meta_predicate(db_add(:)).
:- meta_predicate(db_add_novel(:)).
:- meta_predicate(db_replace(:)).
:- meta_predicate(db_replace(:,:)).
:- meta_predicate(db_replace_novel(:)).
:- meta_predicate(db_replace_novel(:,:)).



db_add(New):-
  assert(New).

%! db_add_novel(+New) is det.
% Asserts the given fact only if it does not already exist.

db_add_novel(New):-
  call(New),
  !.
db_add_novel(New):-
  assert(New).

db_replace(New):-
  new_to_old(New, Old),
  db_replace(Old, New).

%! db_replace(+Old, +New) is det.
% Replaces at most one asserted fact (if present) with another one.

% There is something to overwrite.
db_replace(Old, New):-
  retract(Old),
  !,
  assert(New).
% There is nothing to overwrite.
db_replace(_Old, New):-
  assert(New).

db_replace_novel(New):-
  new_to_old(New, Old),
  db_replace_novel(Old, New).

db_replace_novel(Old, New):-
  call(New),
  \+ call(Old),
  !.
db_replace_novel(Old, New):-
  db_replace(Old, New).

new_to_old(New, Module:Old):-
  strip_module(New, Module, Plain),
  Plain =.. [Predicate | NewArguments],
  length(NewArguments, Length),
  length(OldArguments, Length),
  Old =.. [Predicate | OldArguments].

