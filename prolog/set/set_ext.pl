:- module(
  set_ext,
  [
    arbitrary_intersection/2, % +Sets:list(ordset)
                              % -Intersection:ordset
    arbitrary_union/2, % +Sets:list(ordset)
                       % -Union:ordset
    cardinality/2, % +Set:ordset
                   % -Cardinality:integer
    cartesian_product/2, % +Sets:list(ordset)
                         % -CartesianProduct:ordset(list)
    direct_subset/2, % ?Subset:ordset
                     % +Set:ordset
    emptyset/1, % ?Set:ordset
    are_equinumerous/2, % +Set1:ordset
                        % +Set2:ordset
    minimal_set/2, % ?MinimalSet:ordset
                   % +Sets:ordset(ordset)
    partition/2, % +Set:ordset
                 % -Partition:ordset(ordset)
    powerset/2, % +Set:ordset
                % -Powerset:ordset(ordset)
    strict_subset/2, % ?Subset:ordset
                     % +Superset:ordset
    strict_superset/2, % +Superset:ordset
                       % ?Subset:ordset
    subset2/2, % ?Subset:ordset
               % +Superset:ordset
    superset/2 % +Superset:ordset
               % ?Subset:ordset
  ]
).

/** <module> Set theory extensions

Extra set functions for use in SWI-Prolog.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(error)).
:- use_module(library(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).





%! arbitrary_intersection(+Sets:list(ordset), -Intersection:ordset) is det.

arbitrary_intersection(Sets, Intersection):-
  ord_intersection(Sets, Intersection).



%! arbitrary_union(+Sets:list(ordset), -Union:ordset) is det.

arbitrary_union(Sets, Union):-
  ord_union(Sets, Union).



%! cardinality(+Set:ordset, -Cardinality:nonneg) is det.
% Returns the cardinality of the given set.
%
% @throws type_error If `Set` is not an ordered set.

cardinality(Set, _):-
  \+ is_ordset(Set), !,
  type_error(ordset, Set).
cardinality(Set, Cardinality):-
  length(Set, Cardinality).



%! cartesian_product(
%!   +Sets:list(ordset),
%!   -CartesianProduct:ordset(list)
%! ) is det.

cartesian_product([], []).
cartesian_product([Set|Sets], [H|T]):-
  cartesian_product(Sets, T),
  member(H, Set).



%! direct_subset(+Subset:ordset, +Set:ordset) is semidet.
%! direct_subset(-Subset:ordset, +Set:ordset) is nondet.

direct_subset(Subset, Set):-
  select(_, Set, Subset).



%! emptyset(+Set:ordset) is semidet.
%! emptyset(-Set:ordset) is det.

emptyset([]).



%! are_equinumerous(+Set1:ordset, +Set2:ordset) is semidet.
% Succeeds if the given sets are *equinumerous*, i.e.,
% if they have the same cardinality.
%
% @throws type_error if one of the arguments is not an ordered set.

are_equinumerous(Set1, _):-
  \+ is_ordset(Set1), !,
  type_error(ordset, Set1).
are_equinumerous(_, Set2):-
  \+ is_ordset(Set2), !,
  type_error(ordset, Set2).
are_equinumerous(Set1, Set2):-
  same_length(Set1, Set2).



%! minimal_set(+MinimalSet:ordset, +Sets:ordset(ordset)) is semidet.
%! minimal_set(-MinimalSet:ordset, +Sets:ordset(ordset)) is nondet.
% Set $s \in S$ is a *|minimal set|* of a set of sets $S$ iff
% $S$ contains no strict subset of $s$.

minimal_set(MinimalSet, Compare1):-
  select(MinimalSet, Compare1, Compare2),
  \+((
    member(Subset, Compare2),
    subset2(Subset, MinimalSet)
  )).



%! partition(+Set:ordset, -Partition:ordset(ordset)) is multi.

partition([], []).
partition([H|T], P):-
  partition(T, P1),
  (   select(Xs1, P1, P2),
      ord_add_element(Xs1, H, Xs2),
      ord_add_element(P2, Xs2, P)
  ;   ord_add_element(P1, [H], P)
  ).



%! powerset(+Set:ordset, -Powerset:list(ordset)) is det.

powerset(Set, Powerset):-
  findall(
    Cardinality-Subset,
    (
      subset2(Subset, Set),
      length(Subset, Cardinality)
    ),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Powerset).



%! strict_subset(+Subset:ordset, +Superset:ordset) is semidet.
%! strict_subset(-Subset:ordset, +Superset:ordset) is nondet.
% A *|strict subset|* is a non-equivalent subset.
%
% @throws type_error If `Superset` is not an ordered set.

strict_subset(Subset, Superset):-
  % Predicate subset/2 throws the type_error.
  subset2(Subset, Superset),
  Subset \== Superset.



%! strict_superset(+Superset:ordset, +Subset:ordset) is semidet.
%! strict_superset(+Superset:ordset, -Subset:ordset) is nondet.
% A *|strict superset|* is a non-equivalent superset.
%
% @throws type_error If `Superset` is not an ordered set.

strict_superset(Subset, Superset):-
  % Predicate superset/2 throws the type_error.
  superset(Superset, Subset),
  Superset \== Subset.



%! subset2(+Subset:ordset, +Superset:ordset) is semidet.
%! subset2(-Subset:ordset, +Superset:ordset) is multi.
% Construction proceeds from smaller to greater sublists.
%
% @throws type_error If `Superset` is not an ordered set.

subset2(_, Superset):-
  \+ is_ordset(Superset), !,
  type_error(ordset, Superset).
subset2(Subset, Superset):-
  nonvar(Subset), !,
  ord_subset(Subset, Superset).
subset2(Subset, Superset):-
  sublist(Subset, Superset).



%! superset(+Superset:ordset, +Subset:ordset) is semidet.
%! superset(+Superset:ordset, -Subset:ordset) is multi.
%
% @see Inverse of subset/2.
% @throws type_error If `Superset` is not an ordered set.

superset(Superset, Subset):-
  % Predicate subset/2 throws the type_error.
  subset2(Subset, Superset).
