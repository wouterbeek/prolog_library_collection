:- module(
  relation,
  [
    is_reflexive/1, % +Relation:ugraph
    is_symmetric/1, % +Relation:ugraph
    is_transitive/1, % +Relaton:ugraph
    relation_components/3, % ?Relation:ugraph
                           % ?Set:ordset
                           % ?Pairs:ordset(pair)
    relation_pair/2 % +Relation:ugraph
                    % ?Pair:pair
  ]
).

/** <module> Relation

Support for properties of relations.

@author Wouter Beek
@license MIT license
@version 2015/10
*/

:- use_module(library(graph/s/s_graph)).
:- use_module(library(lambda)).





%! is_reflextive(+Relation:ugraph) is semidet.
% Succeeds if the given binary relation is reflexive.

is_reflexive(Rel):-
  maplist(is_reflexive0, Rel).

is_reflexive0(X-Ns):-
  memberchk(X, Ns).



%! is_symmetric(+Relation:ugraph) is semidet.
% Succeeds if the given relation is symmetric.

is_symmetric(Rel):-
  forall(relation_pair(Rel, X-Y), relation_pair(Rel, Y-X)).



%! is_transitive(+Relation:ugraph) is semidet.
% Suceeds if the given binary relation is transitive.

is_transitive(Rel):-
  forall(
    (relation_pair(Rel, X-Y), relation_pair(Rel, Y-Z)),
    relation_pair(Rel, X-Z)
  ).



%! relation_components(+Rel:ugraph, -Set:ordset, -Pairs:ordset(pair)) is det.
%! relation_components(-Rel:ugraph, +Set:ordset, +Pairs:ordset(pair)) is det.
% Succeeds if Relation relates the elements in Set according to Pairs.

relation_components(Rel, Set, Pairs):-
  s_graph_components(Rel, Set, Pairs).



%! relation_pair(+Relation:ugraph, +Pair:pair) is semidet.
%! relation_pair(+Relation:ugraph, -Pair:pair) is nondet.
% The extension of a binary relation.

relation_pair(Rel, Pair):-
  s_edge(Rel, Pair).
