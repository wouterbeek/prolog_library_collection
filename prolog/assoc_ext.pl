:- module(
  assoc_ext,
  [
    get_assoc_ord_member/3, % +Key, +Assoc, ?Value
    print_assoc/1, % +Assoc
    put_assoc_ord_member/4  % +Key, +OldAssoc, +Value, -NewAssoc
  ]
).
:- reexport(library(assoc)).

/** <module> Association list extension

An association list with multiple values per key, using ordered sets.

@author Wouter Beek
@version 2015/10-2016/01
*/

:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(tree/tree)).




%! assoc_to_tree(+Assoc, -Tree) is det.

assoc_to_tree(Assoc, Tree):-
  assoc_to_tree(Assoc, assoc, Tree).

assoc_to_tree(Assoc, Key, Key-Subtrees):-
  (assoc_to_list(Assoc, Pairs) -> true ; Pairs = []),
  pairs_keys(Pairs, Keys),
  maplist(get_assoc0(Assoc), Keys, Subassocs),
  maplist(assoc_to_tree, Subassocs, Keys, Subtrees).

get_assoc0(Assoc, Key, Subassoc):-
  get_assoc(Key, Assoc, Subassoc).



%! get_assoc_ord_member(+Key, +Assoc:assoc, ?Value) is nondet.

get_assoc_ord_member(Key, Assoc, Val):-
  get_assoc(Key, Assoc, Set),
  % Use ord_memberchk/2 when instantiation is `(+,+,+)'
  % and use member/2 when instantiation is `(+,+,-)'.
  (nonvar(Val) -> ord_memberchk(Val, Set) ; member(Val, Set)).



%! print_assoc(+Assoc) is det.

print_assoc(Assoc):-
  assoc_to_tree(Assoc, Tree),
  print_tree(Tree).



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
