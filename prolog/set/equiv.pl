:- module(
  equiv,
  [
    equiv_class/3, % +EquivalenceRelation:ugraph
                   % +Element
                   % -EquivalenceClass:ordset
    equiv_partition/2, % +EquivalenceRelation:ugraph
                       % -Partition:ordset(ordset)
    is_equiv/1, % +Relation:ugraph
    quotient_set/3, % +EquivalenceRelation:ugraph
                    % +Set:ordset
                    % -QuotientSet:ordset(ordset)
  ]
).

/** <module> Equivalence

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(closure)).
:- use_module(library(error)).
:- use_module(library(graph/s/s_test)).
:- use_module(library(lambda)).
:- use_module(library(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).
:- use_module(library(plunit)).





%! equiv_class(
%!   +EquivalenceRelation:ugraph,
%!   +Element,
%!   -EquivalenceClass:ordset
%! ) is det.
% Returns the equivalence class of Element relative to
% the given EquivalenceRelation.
%
% The function that maps from elements onto their equivalence classes is
% sometimes called the *|canonical projection map|*.
%
% @arg EquivalenceRelation An binary relation that is reflexive,
%      symmetric and transitive, represented as a directed graph.
% @arg Element The element whose equivalence class is returned.
% @arg EquivalenceClass The equivalence class of `Element`.
%      This is an ordered set.

equiv_class(EqRel, X, EqClass):-
  closure(
    % Since an equivalence relation is symmetric,
    % we do not need to use e.g. adjacent/3 here.
    \X^Y^relation_pair(EqRel, X-Y),
    [X],
    EqClass
  ).

:- begin_tests('equiv_class/3').

test(
  'equiv_class(+,+,-) is det. TRUE',
  [forall(equiv_class_test(GName,X,EqClass))]
):-
  s_graph_test(GName, EqRel)
  equiv_class(EqRel, X, EqClass).

equiv_class_test(equiv(1), 1, [1,2,3,4]).
equiv_class_test(equiv(1), 2, [1,2,3,4]).
equiv_class_test(equiv(1), 3, [1,2,3,4]).
equiv_class_test(equiv(1), 4, [1,2,3,4]).

:- end_tests('equiv_class/3').



%! equiv_partition(+EquivalenceRelation:ugraph, -Partition:ordset(ordset)) is det.
%! equiv_partition(-EquivalenceRelation:ugraph, +Partition:ordset(ordset)) is det.

equiv_partition(EqRel, Part):-
  nonvar(EqRel), !,
  maplist(pair_second, EqRel, Part0),
  sort(Part0, Part).
equiv_partition(EqRel, Eqs):-
  instantiation_error(equiv_pairs_partition(EqRel, Eqs)).



%! is_equiv(+Relation:ugraph) is semidet.
% Succeeds if the given relation is an equivalence relation.

is_equiv(Rel):-
  is_reflexive(Rel),
  is_symmetric(Rel),
  is_transitive(Rel).



%! quotient_set(
%!   +EquivalenceRelation:ugraph,
%!   +Set:ordset,
%!   -QuotientSet:ordset(ordset)
%! ) is det.
% Returns the quotient set for `Set`,
% closed under equivalence relation `EquivalenceRelation`.
%
% The quotient set of a set `Set` is the set of all equivalence sets of
% elements in `Set`.
%
% A quotient set of `Set` is also a partition of `Set`.
%
% The standard notation for a quotient set is $S / \approx$.
%
% @arg EquivalenceRelation A (binary) equivalence relation.
%      Represented as a directed graph (see [ugraph]).
% @arg Set An ordered set.
% @arg QuotientSet The quotient set of `Set`.
%      An ordered set.

quotient_set(EqRel, Set, QSet):-
  maplist(equiv_class(EqRel), Set, EqClasses),
  sort(EqClasses, QSet).





% UNIT TESTS %

:- begin_tests(equiv).

test(
  equiv_partition,
  [forall(equiv_partition_test(Pairs,Sets)),true]
):-
  equiv_partition(Pairs, Sets).

% Base case.
equiv_partition_test([], []).
% No multisets.
equiv_partition_test([a-[a,b],b-[a,b]], [[a,b]]).
% Reflexive case.
equiv_partition_test([a-[a]], [[a]]).
% Symmetric case.
equiv_partition_test([a-[a,b]], [[a,b]]).
% Separate sets.
equiv_partition_test([a-[a,b],b-[a,b],c-[c,d],d-[c-d]], [[a,b],[c,d]]).
% Merging sets.
equiv_partition_test([a-[a,b,c,d],b-[a,b,c,d],c-[a,b,c,d],d-[a,b,c,d]], [[a,b,c,d]]).

:- end_tests(equiv).
