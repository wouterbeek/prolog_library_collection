:- module(
  assoc_ext,
  [
    init_assoc/3, % +Keys:list
                  % +IntialValue
                  % -Assoc:assoc
    get_assoc_ord_member/3, % +Key
                            % +Assoc:assoc
                            % ?Value
    put_assoc_ord_member/4 % +Key
                           % +OldAssoc:assoc
                           % +Value
                           % -NewAssoc:assoc
  ]
).

/** <module> Association list extension

An association list with multiple values per key, using ordered sets.

@author Wouter Beek
@version 2013/04-2013/05, 2013/07-2013/10
*/

:- use_module(library(assoc)).
:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(ordsets)).



%! init_assoc(+Keys:list, +IntialValue, -Assoc:assoc) is det.

init_assoc(Keys, IV, A):-
  empty_assoc(A0),
  init_assoc(Keys, A0, IV, A).

init_assoc([], A, _IV, A):- !.
init_assoc([H|T], A1, IV, A3):-
  put_assoc(H, A1, IV, A2),
  init_assoc(T, A2, IV, A3).

%! get_assoc_ord_member(+Key, +Assoc:assoc, ?Value) is nondet.

get_assoc_ord_member(Key, Assoc, Value):-
  get_assoc(Key, Assoc, Set),
  '_old_member'(Value, Set).

%! '_old_member'(+Member, +Set:ordset) is semidet.
%! '_old_member'(-Member, +Set:ordset) is nondet.
% @see Wrapper around ord_memberchk/2 (when instantiation is `(+,+)`)
%      and member/2 (when instantiation is `(-,+)`).

'_old_member'(Value, Ordset):-
  nonvar(Value), !,
  ord_memberchk(Value, Ordset).
'_old_member'(Value, Ordset):-
  member(Value, Ordset).

%! put_assoc_ord_member(
%!   +Key,
%!   +OldAssoc:assoc,
%!   +Value,
%!   -NewAssoc:assoc
%! ) is det.
% Allows multiple values for the same key,
% and stores those values in an ordered set.

% An ordered set already exists as the value of the given key.
put_assoc_ord_member(Key, OldAssoc, Value, NewAssoc):-
  get_assoc(Key, OldAssoc, OldSet), !,
  ord_add_element(OldSet, Value, NewSet),
  put_assoc(Key, OldAssoc, NewSet, NewAssoc),

  % DEB
  length(NewSet, NewSetLength),
  debug(
    assoc_ext,
    'Added <~w,~w> to existing assoc, whose value set cardinality is now ~w.',
    [Key,Value,NewSetLength]
  ).
% The given key has no value, so a new ordered set is created.
put_assoc_ord_member(Key, OldAssoc, Value, NewAssoc):-
  list_to_ord_set([Value], Set),
  put_assoc(Key, OldAssoc, Set, NewAssoc),
  debug(assoc_ext, 'Added <~w,~w> to a new assoc.', [Key,Value]).

/* @tbd
print_assoc(Assoc):-
  print_assoc(0, term_to_atom, Assoc).

:- meta_predicate(print_assoc(+,2,+)).
print_assoc(KeyIndent, KeyTransform, Assoc):-
  is_assoc(Assoc),
  assoc_to_keys(Assoc, Keys),
  ValueIndent is KeyIndent + 1,
  forall(
    member(Key, Keys),
    (
      indent(KeyIndent),
      call(KeyTransform, Key, KeyName),
      format('~w:\n', [KeyName]),
      forall(
        get_assoc(Key, Assoc, Value),
        (
          indent(ValueIndent),
          call(KeyTransform, Value, ValueName),
          format('~w\n', [ValueName])
        )
      )
    )
  ).
*/
