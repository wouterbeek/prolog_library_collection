:- module(
  assoc_ext,
  [
    get_assoc_ord_member/3, % +Key
                            % +Assoc:assoc
                            % ?Value
    put_assoc_ord_member/4 % +Key
                           % +OldAssoc:assoc
                           % +Value
                           % -NewAssoc:assoc
  ]
).
:- reexport(library(assoc)).

/** <module> Association list extension

An association list with multiple values per key, using ordered sets.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).





%! get_assoc_ord_member(+Key, +Assoc:assoc, ?Value) is nondet.

get_assoc_ord_member(Key, Assoc, Val):-
  get_assoc(Key, Assoc, Set),
  % Use ord_memberchk/2 when instantiation is `(+,+)`
  % and use member/2 when instantiation is `(-,+)`.
  (nonvar(Val) -> ord_memberchk(Val, Set) ; member(Val, Set)).



%! put_assoc_ord_member(
%!   +Key,
%!   +OldAssoc:assoc,
%!   +Value,
%!   -NewAssoc:assoc
%! ) is det.
% Allows multiple values for the same key,
% and stores those values in an ordered set.

% An ordered set already exists as the value of the given key.
put_assoc_ord_member(Key, OldAssoc, Val, NewAssoc):-
  get_assoc(Key, OldAssoc, OldSet), !,
  ord_add_element(OldSet, Val, NewSet),
  put_assoc(Key, OldAssoc, NewSet, NewAssoc),

  % DEB
  length(NewSet, NewSetLength),
  debug(
    assoc_ext,
    "Added 〈~w,~w〉 to existing assoc, whose value set cardinality is now ~w.",
    [Key,Val,NewSetLength]
  ).
% The given key has no value, so a new ordered set is created.
put_assoc_ord_member(Key, OldAssoc, Val, NewAssoc):-
  list_to_ord_set([Val], Set),
  put_assoc(Key, OldAssoc, Set, NewAssoc),
  debug(assoc_ext, "Added 〈~w,~w〉 to a new assoc.", [Key,Val]).
