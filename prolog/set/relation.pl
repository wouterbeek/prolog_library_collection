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

:- use_module(library(closure)).
:- use_module(library(chr)).
:- use_module(library(flag_ext)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(lambda)).

:- chr_constraint(leq/2).

antisymmetry @ leq(X,Y), leq(Y,X)  <=> switch(antisymm) | X = Y    .
idempotence  @ leq(X,Y) \ leq(X,Y) <=>                    true     .
reflexivity  @ leq(X,X)            <=> switch(refl)     | true     .
symmetry     @ leq(X,Y)            ==> switch(symm)     | leq(Y,X) .
transitivity @ leq(X,Y), leq(Y,Z)  ==> switch(trans)    | leq(X,Z) .

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
  maplist(enable_switch, [refl]),
  maplist(disable_switch, [antisymm,symm,trans]),
  relation_compound(Rel, _, Pairs),
  maplist(translate_term0, Pairs, Es),
  call(rel, Es),
  relation_closure(
    \Pairs^Result^(
      member(Pair, Pairs),
      (   Result = Pair
      ;   pair_element(Pair, X),
          Result = X-X
      )
    ),
    Rel,
    ReflRel
  ).

translate_term0(X-Y, leq(X,Y)).



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
  relation_components(Rel, _, Pairs),
  maplist(call, Pairs),
  relation_closure(
    \Pairs^Result^(
      member(Pair, Pairs),
      (   Result = Pair
      ;   inverse_pair(Pair, Result)
      )
    ),
    Rel,
    SymmRel
  ).



%! transitive_closure(+Relation:ugraph, -TransitiveRelation:ugraph) is det.
% Transitive closure over a srep graph / ugraph could have been implemented
% using relational_closure/3:

transitive_closure(Rel, TransRel):-
  relation_closure(
    \Pairs^Result^(
        member(Result, Pairs)
    ;   member(X-Y, Pairs),
        member(Y-Z, Pairs),
        Result = X-Z
    ),
    Rel,
    TransRel
  ).
