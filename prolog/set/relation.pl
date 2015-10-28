:- module(
  relation,
  [
    is_reflexive/1, % +Relation:ugraph
    is_symmetric/1, % +Relation:ugraph
    is_transitive/1, % +Relaton:ugraph
    reflexive_closure/2, % +Relation:ugraph
                         % -Closure:ugraph
    relation_components/3, % ?Relation:ugraph
                           % ?Set:ordset
                           % ?Pairs:ordset(pair)
    relation_pair/2, % +Relation:ugraph
                     % ?Pair:pair
    symmetric_closure/2, % +Relation:ugraph
                         % -Closure:ugraph
    transitive_closure/2 % +Relation:ugraph
                         % -TransitiveRelation:ugraph
  ]
).

/** <module> Relation

Support for properties of relations.

@author Wouter Beek
@license MIT license
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(closure)).
:- use_module(library(graph/graph_test)).
:- use_module(library(graph/s/s_edge)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).

:- meta_predicate(relational_closure(+,2,-)).





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



%! reflexive_closure(+Relation:ugraph, -ReflexiveRelation:ugraph) is det.

reflexive_closure(Rel, ReflRel):-
  relational_closure(
    Rel,
    \Pairs^Result^(
      member(Pair, Pairs),
      (   Result = Pair
      ;   pair_element(Pair, X),
          Result = X-X
      )
    ),
    ReflRel
  ).



%! relation_components(+Relation:ugraph, -Set:ordset, -Pairs:ordset(pair)) is det.
%! relation_components(-Relation:ugraph, +Set:ordset, +Pairs:ordset(pair)) is det.
% Succeeds if Relation relates the elements in Set according to Pairs.

relation_components(Rel, Set, Pairs):-
  s_graph_components(Rel, Set, Pairs).



%! relation_pair(+Relation:ugraph, +Pair:pair) is semidet.
%! relation_pair(+Relation:ugraph, -Pair:pair) is nondet.
% The extension of a binary relation.

relation_pair(Rel, Pair):-
  s_edge(Rel, Pair).



%! symmetric_closure(+Relation:ugraph, -SymmetryRelation:ugraph) is det.

symmetric_closure(Rel, SymmRel):-
  relational_closure(
    Rel,
    \Pairs^Result^(
      member(Pair, Pairs),
      (   Result = Pair
      ;   inverse_pair(Pair, Result)
      )
    ),
    SymmRel
  ).



%! transitive_closure(+Relation:ugraph, -TransitiveRelation:ugraph) is det.
% Transitive closure over a srep graph / ugraph could have been implemented
% using relational_closure/3:

transitive_closure(Rel, TransRel):-
  relational_closure(
    Rel,
    \Pairs^Result^(
        member(Result, Pairs)
    ;   member(X-Y, Pairs),
        member(Y-Z, Pairs),
        Result = X-Z
    ),
    TransRel
  ).





% HELPERS %

%! relational_closure(
%!   +Relation:ugraph,
%!   :Goal,
%!   -ClosedRelation:ugraph
%! ) is det.
% Allows the calculation of the closure of a relation directly.
% Internally, the closure is calculated for the extension of the relation,
% i.e., its edge pairs.
%
% The mode is the same as for `Goal`.

relational_closure(Rel, Goal_2, ClosedRel):-
  relation_components(Rel, Set, Pairs),
  closure(Goal_2, Pairs, ClosedPairs),
  relation_components(ClosedRel, Set, ClosedPairs).
