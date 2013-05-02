:- module(
  rdfs_entailment,
  [
    rdf_rdfs/3, % ?S:oneof([bnode,uri])
                % ?P:uri
                % ?O:oneof([bnode,literal,uri])
    rdf_rdfs/4 % ?S:oneof([bnode,uri])
               % ?P:uri
               % ?O:oneof([bnode,literal,uri])
               % ?G:atom
  ]
).

/** <module> RDFS ENTAILMENT

RDFS entailment extensions of simple entailment, as defined in RDF-SEMANTICS.

---+ TODO

---++ rdf_reachable/[3,5]

rdf_reachable/[3,5] does not have a graphs argument.

---++ Relation between subproperty and subclass hierarchies?

Can the following valid inference be used for RDFS entailment?

If <x,rdfs:subPropertyOf,y> is true, then IEXT(x) \subseteq IEXT(y).

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_entailment)).

:- rdf_meta(rdf_rdfs(r,r,r)).
:- rdf_meta(rdf_rdfs(r,r,r,?)).



rdf_rdfs(S, P, O):-
  rdf_rdfs(S, P, O, _).

% Full conformity with RDF entailment.
rdf_rdfs(S, P, O, Gs):-
  rdf_rdf(S, P, O, Gs).
% Individuals based on RDFS domain information.
rdf_rdfs(U, rdf:type, Y, Gs):-
  rdf_rdfs(X, rdfs:domain, Y, Gs),
  rdf_rdfs(U, X, _V, Gs).
% Individuals based on RDFS range information.
rdf_rdfs(V, rdf:type, Y, Gs):-
  rdf_rdfs(X, rdfs:range, Y, Gs),
  rdf_rdfs(_U, X, V, Gs).
% Subject and object terms for predicate rdfs:subPropertyOf are individuals
% of RDF property.
rdf_rdfs(P, rdf:type, rdf:'Property', Gs):-
  % Transitivity of the subproperty relation is not needed in order to
  % find the property individuals.
  rdf_rdf(P1, rdfs:subPropertyOf, P2, Gs),
  (P1 == P2 -> P = P1 ; (P = P1 ; P = P2)),
  \+ rdf_rdf(P, rdf:type, rdf:'Property', Gs).
% Reflexivity of the subproperty relation.
rdf_rdfs(P, rdfs:subPropertyOf, P, Gs):-
  rdf_rdfs(P, rdf:type, rdf:'Property', Gs).
% Transitivity of the subproperty relation.
rdf_rdfs(P1, rdfs:subPropertyOf, P2, Gs):-
  rdf_rdfs(P1, rdf:type, rdf:'Property', Gs),
  rdf_rdfs(P2, rdf:type, rdf:'Property', Gs),
  P1 \== P2,
  rdf_reachable(P1, rdfs:subPropertyOf, P2),
  % Direct subproperty relations were already found with rdf_rdf/4.
  % Only the indirectly / transitively linked subproperties are
  % considered here.
  \+ rdf_rdf(P1, rdfs:subPropertyOf, P2).
rdf_rdfs(S, P2, O, Gs):-
  rdf_reachable(P1, rdfs:subPropertyOf, P2),
  % For the reflexive case rdf_rdf/4 has already provided the triples.
  P1 \== P2,
  rdf_rdfs(S, P1, O, Gs),
  % The triple was not already RDF-entailed.
  \+ rdf_rdf(S, P2, O, Gs).
%rdf_rdfs(P1, rdfs:subClassOf, P2, Gs):-
