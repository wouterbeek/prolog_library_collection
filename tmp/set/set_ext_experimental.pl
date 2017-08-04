:- module(
  set_ext_experimental,
  [
    binary_subsets/2, % +Universe:list
                      % -Subsets:list(list(boolean))
    delete_supersets/4 % +Original:ordset(ordset)
                       % +Compare:ordset(ordset)
                       % -Result:ordset(ordset)
                       % -Rest:ordset(ordset)
  ]
).

/** <module> Set theory extensions: experimental

Predicates for set theory that are considered experimental,
i.e. not (yet) ready for use in applications.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(set/set_ext)).

:- meta_predicate(call_complete(2,+,-)).
:- meta_predicate(call_det(0)).
:- meta_predicate(call_det(1,+)).
:- meta_predicate(call_det(2,+,+)).
:- meta_predicate(call_det(3,+,+,+)).
:- meta_predicate(call_det(4,+,+,+,+)).
:- meta_predicate(call_det(5,+,+,+,+,+)).
:- meta_predicate(call_det(6,+,+,+,+,+,+)).
:- meta_predicate(call_det(7,+,+,+,+,+,+,+)).
:- meta_predicate(call_mode(+,0)).
:- meta_predicate(call_nth(0,-)).
:- meta_predicate(call_semidet(0)).
:- meta_predicate(enforce_mode(0,+,+)).





%! binary_subsets(+Set:ordset, -Subsets:list(list(bit))) is det.
% Returns all subsets of the given set as a list of binary lists.
%
% @arg Set An ordered set.
% @arg Subsets A list of bitlists representing subsets of =Set=.
%
% @tbd What is this?

binary_subsets(Set, Subsets):-
  cardinality(Set, Cardinality),
  repeating_list(0, Cardinality, BinarySet),
  call_complete(next_binary_subset, BinarySet, BinarySubsets),
  maplist(binary_overlay(Set), BinarySubsets, Subsets).



%! delete_supersets(
%!   +Original:ordset(ordset),
%!   +Compare:ordset(ordset),
%!   -Result:ordset(ordset),
%!   -Rest:ordset(ordset)
%! ) is det.
% Splits the `Original` sets into those that are and those that are not
% a superset of some member of `Compare`.
%
% @arg Original The original sets.
% @arg Compare The sets to compare with.
% @arg Result Supersets of some compare set.
% @arg Rest Strict subsets of some compare set.
%
% @tbd What is this?

delete_supersets(Original, Compare, Result, Rest):-
  partition(contains_superset_of(Compare), Original, Rest, Result).
contains_superset_of(Compare, Set):-
  member(Superset, Compare),
  superset(Superset, Set), !.



%! next_binary_subset(+Subset:list(bit), -NextSubset:list(bit)) is det.
% Returns the next subset.
%
% Subsets are represented as lists of bits.
%
% Positions in the list correspond to potential elements in the set.
% For example
% ```prolog
% [1,0,0,1,1,0]
% ```
% may denote the set
% ```
% {a,d,e}
% ```
%
% @arg Subset A list of bits.
% @arg NextSubset A list of bits.

next_binary_subset([0|T], [1|T]).
next_binary_subset([1|T1], [0|T2]):-
  next_binary_subset(T1, T2).





% HELPERS %

%! args_instantiation(+Arguments:list, -Instiations:list) is det.

args_instantiation([], []).
args_instantiation([H|T1], [+|T2]):- !,
  nonvar(H),
  args_instantiation(T1, T2).
args_instantiation([H|T1], [-|T2]):- !,
  var(H),
  args_instantiation(T1, T2).
args_instantiation([_|T1], [_|T2]):-
  args_instantiation(T1, T2).



%! binary_overlay(+Original:list, +Overlay:list, -Result:list) is det.
%! binary_overlay(+Original:list, -Overlay:list, +Result:list) is det.
%! binary_overlay(+Original:list, ?Overlay:list, ?Result:list) is nondet.
% The result is the sublist of the original list,
%  where the overlay decides on which elements are kept (`1`)
%  and which are not (`0`).

binary_overlay(Original, Overlay, Result):-
  enforce_mode(
    '_binary_overlay'(Original, Overlay, Result),
    [Original,Overlay,Result],
    [[+,+,+]-semidet,[+,+,-]-det,[+,-,+]-det]
  ).
'_binary_overlay'([], [], []).
'_binary_overlay'([H|T1], [1|Overlay], [H|T2]):-
  '_binary_overlay'(T1, Overlay, T2).
'_binary_overlay'([_|T1], [0|Overlay], T2):-
  '_binary_overlay'(T1, Overlay, T2).



%! call_complete(:Goal, +Input, -Results:list) is det.
% Runs the given goal on the given input until it wears out.
% The goal is enforced to be deteministic or semi-deterministic
% (the extra choicepoint is automatically dropped).
%
% @tbd Check whether this can be unified with multi/[4,5].

call_complete(Goal, Input, [Input|History]):-
  once(call(Goal, Input, Output)),
  Input \== Output, !,
  call_complete(Goal, Output, History).
call_complete(_, Input, [Input]).



call_det(Goal_0):-
  catch(Goal_0, _, mode_error(det, Goal_0)).

call_det(Goal_1, Type1-Arg1):-
  is_of_type(Type1, Arg1), !,
  call(Goal_1, Arg1), !.
call_det(Goal_1, _-Arg1):-
  call(Goal_1, Arg1).

call_det(Goal_2, Type1-Arg1, Type2-Arg2):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2), !,
  call(Goal_2, Arg1, Arg2), !.
call_det(Goal_2, _-Arg1, _-Arg2):-
  call(Goal_2, Arg1, Arg2).

call_det(Goal_3, Type1-Arg1, Type2-Arg2, Type3-Arg3):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3), !,
  call(Goal_3, Arg1, Arg2, Arg3), !.
call_det(Goal_3, _-Arg1, _-Arg2, _-Arg3):-
  call(Goal_3, Arg1, Arg2, Arg3).

call_det(Goal_4, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4), !,
  call(Goal_4, Arg1, Arg2, Arg3, Arg4), !.
call_det(Goal_4, _-Arg1, _-Arg2, _-Arg3, _-Arg4):-
  call(Goal_4, Arg1, Arg2, Arg3, Arg4).

call_det(Goal_5, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4, Type5-Arg5):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4),
  is_of_type(Type5, Arg5), !,
  call(Goal_5, Arg1, Arg2, Arg3, Arg4, Arg5), !.
call_det(Goal_5, _-Arg1, _-Arg2, _-Arg3, _-Arg4, _-Arg5):-
  call(Goal_5, Arg1, Arg2, Arg3, Arg4, Arg5).

call_det(Goal_6, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4, Type5-Arg5, Type6-Arg6):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4),
  is_of_type(Type5, Arg5),
  is_of_type(Type6, Arg6), !,
  call(Goal_6, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), !.
call_det(Goal_6, _-Arg1, _-Arg2, _-Arg3, _-Arg4, _-Arg5, _-Arg6):-
  call(Goal_6, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6).

call_det(Goal_7, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4, Type5-Arg5, Type6-Arg6, Type7-Arg7):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4),
  is_of_type(Type5, Arg5),
  is_of_type(Type6, Arg6),
  is_of_type(Type7, Arg7), !,
  call(Goal_7, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7), !.
call_det(Goal_7, _-Arg1, _-Arg2, _-Arg3, _-Arg4, _-Arg5, _-Arg6, _-Arg7):-
  call(Goal_7, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7).



%! call_mode(+Mode:atom, :Goal_0) .

call_mode(det, Goal_0):- !,
  call_det(Goal_0).
call_mode(semidet, Goal_0):- !,
  call_semidet(Goal_0).
call_mode(_, Goal_0):-
  call(Goal_0).



%! call_nth(:Goal_0, +N:nonneg) is semidet.
% Calls the given goal the given number of times.
%
% This does not exclude the case in which the goal
%  could have been executed more than `N` times.
%
% @arg Goal_0 A nondeterministic goal.
% @arg N A nonnegative integer.
%
% @author Ulrich Neumerkel

call_nth(Goal_0, C):-
  State = count(0),
  Goal_0,
  arg(1, State, C1),
  C2 is C1 + 1,
  nb_setarg(1, State, C2),
  C = C2.



%! call_semidet(:Goal_0) is det.
% Executes the given semi-deterministic goal exactly once,
%  i.e., regardless of any open choice points.
% If the goal is not semideterministic, an error is thrown.
%
% @author Ulrich Neumerkel
% @error error(mode_error(semidet, Goal_0),
%        context(call_semidet/1, 'Message left empty.'))

call_semidet(Goal_0):-
  (   call_nth(Goal_0, 2)
  ->  mode_error(semidet, Goal_0)
  ;   once(Goal_0)
  ).



%! enforce_mode(:Goal_0, +Arguments, +Declaration) .

enforce_mode(Goal_0, Args, Declaration):-
  member(Instantiation-Mode, Declaration),
  args_instantiation(Args, Instantiation), !,
  call_mode(Mode, Goal_0).
enforce_mode(Goal_0, _, _):-
  call_mode(_, Goal_0).



%! mode_error(+Mode:oneof([det,nondet,semidet]), +Goal:term) is det.
% Throws a mode error.
% This happens when a goal does not have the required mode.

mode_error(Mode, Goal):-
  format(string(Reason), "Goal ~k does not have mode ~a.", [Goal,Mode]),
  throw(error(mode_error(Reason), _)).
