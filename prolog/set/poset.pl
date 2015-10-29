:- module(
  poset,
  [
    identity/3, % +Poset:ugraph
                % ?Element1
                % ?Element2
    maximal/2, % +Poset:ugraph
               % ?Maximal
    maximum/2, % +Poset:ugraph
               % ?Maximum
    minimal/2, % +Poset:ugraph
               % ?Minimal
    minimum/2, % +Poset:ugraph
               % ?Minimum
    poset_components/3 % ?Poset:ugraph
                       % ?Set:ordset
                       % ?Order:ordset(pair)
  ]
).

/** <module> Poset

Support for partially ordered sets.

Design decisions:
  * Posets are represented by ugraphs (see [ugraph]).

@author Wouter Beek
@license MIT license
@tbd Implement height/2 where the height of an element is the lenght of
     the longest chain from `Element` to a minimal element.
@version 2015/10
*/

:- use_module(library(graph/graph_walk)).
:- use_module(library(graph/s/s_edge)).
:- use_module(library(graph/s/s_vertex)).
:- use_module(library(set/relation)).





%! identity(+Poset:ugraph, +Element1, +Element2) is semidet.
%! identity(+Poset:ugraph, +Element1, -Element2) is nondet.
%! identity(+Poset:ugraph, -Element1, +Element2) is nondet.
%! identity(+Poset:ugraph, -Element1, -Element2) is nondet.
% Succeeds for Element1 and Element2 that are identical
% according to the partially ordered set Poset.

% Reflexivity (x = x).
identity(Poset, E, E):- !,
  s_vertex(Poset, E).
% (x ≤ y) and (y ≤ x) implies (x = y).
identity(Poset, E1, E2):-
  s_edge(Poset, E1-E2),
  s_edge(Poset, E2-E1).


%! maximal(+Poset:ugraph, +Maximal) is semidet.
%! maximal(+Poset:ugraph, -Maximal) is nondet.
% Succeds for Maximal elements in partially ordered set Poset.

maximal(Poset, Maximal):-
  s_vertex(Poset, Maximal),
  forall(
    walk(s_edges, s_neighbor, Poset, Maximal, E),
    identity(Poset, Maximal, E)
  ).


%! maximum(+Poset:ugraph, +Maximum) is semidet.
%! maximum(+Poset:ugraph, -Maximum) is semidet.
% Succeds for the maximum element (if available)
% in partially ordered set Poset.

maximum(Poset, Maximum):-
  maximal(Poset, Maximum),
  forall(
    maximal(Poset, Maximal),
    Maximal == Maximum
  ).



%! minimal(+Poset:ugraph, +Minimal) is semidet.
%! minimal(+Poset:ugraph, -Minimal) is nondet.
% Succeeds for Minimal elements of partially ordered set Poset.

minimal(Poset, Minimal):-
  s_vertex(Poset, Minimal),
  forall(
    walk(s_edges, s_neighbor, Poset, Element, Minimal),
    identity(Poset, Element, Minimal)
  ).


%! minimum(+Poset:ugraph, +Minimum) is semidet.
%! minimum(+Poset:ugraph, -Minimum) is semidet.
% Succeds for the minimum element (if available)
% in partially ordered set Poset.

minimum(Poset, Minimum):-
  minimal(Poset, Minimal),
  forall(
    minimal(Poset, Minimal),
    Minimal == Minimum
  ).



%! is_partial_order(+Order:list(pair)) is semidet.
% Succeeds if the given order is
%   1. reflexive,
%   2. antisymmetric, and
%   3. transitive.
% @tbd How to implement the check for anti-symmetry?

partial_order(Order):-
  is_reflexive(Order),
  is_transitive(Order).



%! poset_components(+Poset:ugraph, -Set:ordset, -Order:ordset(pair)) is det.
%! poset_components(-Poset:ugraph, +Set:ordset, +Order:ordset(pair)) is det.
% Here we make use of the fact that posets are represented as ugraphs.

poset_components(Poset, Set, Order):-
  relation_components(Poset, Set, Order).
