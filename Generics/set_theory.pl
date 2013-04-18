:- module(
  set_theory,
  [
    cardinality/2, % +S:ord_set
                   % -C:integer
    delete_sets/5, % +Sets:ord_set(ord_set(object))
                   % +CompareSets:ord_set(ord_set(object))
                   % +Comparator:atom
                   % -ResultSets:ord_set(ord_set(object))
                   % -RestSets:ord_set(ord_set(object))
    delete_supersets/4, % +Sets:ord_set(ord_set(object))
                        % +CompareSets:ord_set(ord_set(object))
                        % -ResultSets:ord_set(ord_set(object))
                        % -RestSets:ord_set(ord_set(object))
    equinumerous/2, % +S:ord_set
                    % +T:ord_set
    is_minimal/2, % +Set:ord_set(object)
                  % +Sets:ord_set(ord_set(object))
    next_subset/2, % +Set:list(boolean)
                   % -NextSet:list(boolean)
    subsets/2, % +Universe:list(object)
               % -Subsets:list(list(boolean))
    transitive_closure/3 % +Predicate:atom
                         % +Input:object
                         % -Outputs:ord_set(object)
  ]
).

/** <module> Set theory

Extra set functions for use in SWI-Prolog.

@author Wouter Beek
@version 2011/11-2011/12, 2012/02, 2012/08, 2012/10
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).

:- meta_predicate delete_sets(+,+,2,-,-).
:- meta_predicate transitive_closure(2,+,-).



%% cardinality(+S:ord_set, -C:integer) is det.
% Returns the cardinality of the given set.
%
% @param S An ordered set.
% @param An integer representing the number of elements in set =S=.

cardinality(S, C):-
  length(S, C).

%% delete_sets(
%%   +Sets:set(set(object)),
%%   +CompareSets:set(set(object)),
%%   +Comparator:atom,
%%   -ResultSets:set(set(object)),
%%   -RestSets:set(set(object))
%% ) is det.
% Deletes from =Sets= all sets that fail to compare to some member of
% =CompareSets=.
% The comparison is performed by the binary semideterministic predicate
% =Comparator=.
%
% @param Sets A set of sets.
% @param CompareSets A set of sets.
% @param Comparator The atomic name of a binary semideterministic
%        predicate.
% @param ResultSets A set of sets.
% @param RestSets A set of sets.
% @see An example of this, using comparator subset/2, can be found
%      in delete_supersets/4.

delete_sets([], _CompareSets, _Comparator, [], []):-
  % The empty set has no supersets that need to be removed.
  !.
delete_sets(
  [Set | Sets],
  CompareSets,
  Comparator,
  ResultSets,
  [Set | RestSets]
):-
  member(CompareSet, CompareSets),
  % If the comparison succeeds, then =Set= is exclused from the results
  % (and included in the rest).
  call(Comparator, CompareSet, Set),
  !,
  delete_sets(Sets, CompareSets, Comparator, ResultSets, RestSets).
delete_sets(
  [Set | Sets],
  CompareSets,
  Comparator,
  [Set | ResultSets],
  RestSets
):-
  % If the comparison does not succeed, then =Set= is included in the
  % results (and excluded from the rest).
  delete_sets(Sets, CompareSets, Comparator, ResultSets, RestSets).

%% delete_supersets(
%%   +Sets:set(set(object)),
%%   +CompareSets:set(set(object)),
%%   -ResultSets:set(set(object)),
%%   -RestSets:set(set(object))
%% ) is det.
% Deletes from =Sets= all sets that are a superset of some member of
% =CompareEnvironments=.
%
% @param Sets A set of sets.
% @param CompareSets A set of sets.
% @param ResultSets A set of sets.
% @param RestSets A set of sets.

delete_supersets(Sets, CompareSets, ResultSets, RestSets):-
  delete_sets(Sets, CompareSets, subset, ResultSets, RestSets).

%% equinumerous(+S:ord_set, +T:ord_set) is semidet.
% Succeeds if sets =S= and =T= are equinumerous, i.e., if they have
% the same cardinality.
%
% @param S An ordered set.
% @param T An ordered set.

equinumerous(S, T):-
  cardinality(S, C),
  cardinality(T, C).

%% is_minimal(+Set:set, +Sets:set(set)) is semidet.
% Succeeds if =Set= is a minimal set with respect to =Sets=.
%
% @param Set A set of unique objects.
% @param Sets A set of sets.

is_minimal(Set, Sets):-
  \+((
    member(CompareSet, Sets),
    subset(CompareSet, Set)
  )).

%% next_subset(+Set:list(bit), -NextSet:list(bit)) is det.
% Returns the next subset. Subsets are represented as lists of bits.
% Positions in the list correspond to potential elements in the set.
%
% @param Set A list of bits.
% @param NextSet A list of bits.

next_subset([0 | T], [1 | T]).
next_subset([1 | T1], [0 | T2]):-
  next_subset(T1, T2).

%% subsets(+Set:ord_set, -Subsets:list(list(bit))) is det.
% Returns all subsets of the given set as a list of binary lists.
%
% @param Set An ordered set.
% @param Subsets A list of bitlists representing subsets of =Set=.

subsets(Set, Subsets):-
  length(Set, L),
  repeating_list(0, L, Input),
  complete(next_subset, Input, Subsets).

%% transitive_closure(
%%   +Predicate:atom,
%%   +Input:list(term),
%%   -Outputs:list(term)
%% ) is det.
% Returns the transitive closure of =Predicate= applied to =Input=.
%
% @param Predicate The atomic name of a predicate.
% @param Input Either a term or a list of terms.
% @param Outputs A list of terms. This is the transitive closure.

transitive_closure(Predicate, Input, Outputs):-
  \+(is_list(Input)),
  !,
  transitive_closure(Predicate, [Input], Outputs).
transitive_closure(_Predicate, [], []).
transitive_closure(Predicate, [Input | Inputs], Outputs):-
  Goal =.. [Predicate, Input, Intermediaries],
  call(Goal),
  ord_union(Intermediaries, Inputs, Inputs_),
  transitive_closure(Predicate, Inputs_, Outputs_),
  ord_union(Outputs_, Intermediaries, Outputs).

