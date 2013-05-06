:- module(
  rdf_axioms,
  [
    materialize/0,
    rdfax/3,
    test_triple/3
  ]
).

/** <module> RDF AXIOMS

An axiomatic approach towards RDF(S) materialization.

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_namespace)).

:- rdf_meta(rdfax(r,r,r)).
:- rdf_meta(rdfax0(r,r,r)).
:- rdf_meta(test_triple(r,r,r)).

:- rdf_register_namespace(rdf).
:- rdf_register_namespace(rdfs).



materialize:-
  setoff(
    S/P/O,
    (
      rdfax(S, P, O),
      \+ rdfax0(S, P, O)
    ),
    Triples
  ),
  forall(
    member(S/P/O, Triples),
    rdf_assert(S, P, O)
  ).

% [RDFS-12] Individuals of =|rdfs:'Datatype'|= are subclasses of
%           =|rdfs:'Literal'|=.
rdfax(X, rdfs:subClassOf, rdfs:'Literal'):-
  rdfax0(X, rdf:type, rdfs:'Datatype').
% [RDFS-11] Individuals of =|rdfs:'ContainerMembershipProperty'|= are
%           subproeprties of =|rdf:member|=.
rdfax(X, rdfs:subPropertyOf, rdfs:member):-
  rdfax0(X, rdf:type, rdfs:'ContainerMembershipProperty').
% [RDFS-10b] Transitivity of subclass relation.
rdfax(C1, rdfs:subClassOf, C3):-
  rdfax0(C1, rdfs:subClassOf, C2),
  C1 \== C2,
  rdfax0(C2, rdfs:subClassOf, C3),
  C2 \== C3.
% [RDFS-10a] Reflexivity of subclass relation.
rdfax(C, rdfs:subClassOf, C):-
  rdfax0(C, rdf:type, rdfs:'Class').
% [RDFS-9b] Individuals are closed under transitivity of the
%           subclass hierarchy.
rdfax(X, rdf:type, C2):-
  rdfax0(C1, rdfs:subClassOf, C2),
  rdfax0(X, rdf:type, C1).
% [RDFS-9a] Resources that occur in the subclass hierarchy are
%           individuals of =|rdfs:'Class'|=.
rdfax(C, rdf:type, rdfs:'Class'):-
  rdfax0(C1, rdfs:subClassOf, C2),
  (C = C1 ; C = C2).
% [RDFS-8] Classes are subclasses of =|rdfs:'Resource'|=.
rdfax(C, rdfs:subClassOf, rdfs:'Resource'):-
  rdfax0(C, rdf:type, rdfs:'Class').
% [RDFS-7b] Use the subproperty hierarchy to derive an arbitrary triple.
rdfax(S, P2, O):-
  rdfax0(P1, rdfs:subPropertyOf, P2),
  rdfax0(S, P1, O).
% [RDFS-7a] Resources that occur in the subproperty relation are
%           individuals of =|rdf:'Property'|=.
rdfax(P, rdf:type, rdf:'Property'):-
  rdfax0(P1, rdfs:subPropertyOf, P2),
  (P = P1 ; P = P2).
% [RDFS-6a] Transitivity of subproperty relation.
rdfax(X, rdfs:subPropertyOf, Z):-
  rdfax0(X, rdfs:subPropertyOf, Y),
  X \== Y,
  rdfax0(Y, rdfs:subPropertyOf, Z),
  Y \== Z.
% [RDFS-6a] Reflexivity of subproperty relation.
rdfax(X, rdfs:subPropertyOf, X):-
  rdfax0(X, rdf:type, rdfs:'Property').
% [RDFS-5] If <P,rdfs:range,R> and <X,P,Y>, then <Y,rdf:type,R>.
rdfax(Y, rdf:type, R):-
  rdfax0(P, rdfs:range, R),
  rdfax0(_, P, Y).
% [RDFS-4] If <P,rdfs:domain,D> and <X,P,Y>, then <X,rdf:type,D>.
rdfax(X, rdf:type, D):-
  rdfax0(P, rdfs:domain, D),
  rdfax0(X, P, _).
% [RDFS-3] Plain literals are individuals of =|rdfs:'Literal'|=.
rdfax(Lit, rdf:type, rdfs:'Literal'):-
  rdfax0(_, _, literal(Lit)).
% [RDFS-2] Everything is an =|rdfs:'Resource'|=.
rdfax(X, rdf:type, rdfs:'Resource'):-
  rdfax0(S, P, O),
  (X = S ; X = P ; X = O).
% [RDFS-1] If a resource has in instance, then it must be an =|rdfs:'Class'|=.
rdfax(Y, rdf:type, rdfs:'Class'):-
  rdfax0(_, rdf:type, Y).
% [RDF-1] Terms that occur in the predicate position are instances of
%         =|rdf:'Property'|=.
rdfax(X, rdf:type, rdf:'Property'):-
  rdfax0(_, X, _).
% [RDF-2] XML literals are instances of =|rdf:'XMLLiteral'|=.
rdfax(Lit, rdf:type, rdf:'XMLLiteral'):-
  rdf_global_id(rdf:'XMLLiteral', Type),
  rdfax0(_, _, literal(type(Type, Lit))).
% [RDF-3] RDF axiomatic triples.
rdfax0(rdf:type, rdf:type, rdf:'Property').
rdfax0(rdf:subject, rdf:type, rdf:'Property').
rdfax0(rdf:predicate, rdf:type, rdf:'Property').
rdfax0(rdf:object, rdf:type, rdf:'Property').
rdfax0(rdf:first, rdf:type, rdf:'Property').
rdfax0(rdf:rest, rdf:type, rdf:'Property').
rdfax0(rdf:value, rdf:type, rdf:'Property').
rdfax0(rdf:'_1', rdf:type, rdf:'Property').
rdfax0(rdf:'_2', rdf:type, rdf:'Property').
rdfax0(rdf:'_3', rdf:type, rdf:'Property').
/*
rdfax0(UriRef, rdf:type, rdf:'Property'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
*/
rdfax0(rdf:nil, rdf:type, rdf:'List').
% [RDFS-13a] RDFS axiomatic triples: domains.
rdfax0(rdf:type, rdfs:domain, rdfs:'Resource').
rdfax0(rdfs:domain, rdfs:domain, rdf:'Property').
rdfax0(rdfs:range, rdfs:domain, rdf:'Property').
rdfax0(rdfs:subPropertyOf, rdfs:domain, rdf:'Property').
rdfax0(rdfs:subClassOf, rdfs:domain, rdfs:'Class').
rdfax0(rdf:subject, rdfs:domain, rdf:'Statement').
rdfax0(rdf:predicate, rdfs:domain, rdf:'Statement').
rdfax0(rdf:object, rdfs:domain, rdf:'Statement').
rdfax0(rdfs:member, rdfs:domain, rdfs:'Resource').
rdfax0(rdf:first, rdfs:domain, rdf:'List').
rdfax0(rdf:rest, rdfs:domain, rdf:'List').
rdfax0(rdfs:seeAlso, rdfs:domain, rdfs:'Resource').
rdfax0(rdfs:isDefinedBy, rdfs:domain, rdfs:'Resource').
rdfax0(rdfs:comment, rdfs:domain, rdfs:'Resource').
rdfax0(rdfs:label, rdfs:domain, rdfs:'Resource').
rdfax0(rdf:value, rdfs:domain, rdfs:'Resource').
% [RDFS-13b] RDFS axiomatic triples: range.
rdfax0(rdf:type, rdfs:range, rdfs:'Class').
rdfax0(rdfs:domain, rdfs:range, rdfs:'Class').
rdfax0(rdfs:range, rdfs:range, rdfs:'Class').
rdfax0(rdfs:subPropertyOf, rdfs:range, rdf:'Property').
rdfax0(rdfs:subClassOf, rdfs:range, rdfs:'Class').
rdfax0(rdf:subject, rdfs:range, rdfs:'Resource').
rdfax0(rdf:predicate, rdfs:range, rdfs:'Resource').
rdfax0(rdf:object, rdfs:range, rdfs:'Resource').
rdfax0(rdfs:member, rdfs:range, rdfs:'Resource').
rdfax0(rdf:first, rdfs:range, rdfs:'Resource').
rdfax0(rdf:rest, rdfs:range, rdf:'List').
rdfax0(rdfs:seeAlso, rdfs:range, rdfs:'Resource').
rdfax0(rdfs:isDefinedBy, rdfs:range, rdfs:'Resource').
rdfax0(rdfs:comment, rdfs:range, rdfs:'Literal').
rdfax0(rdfs:label, rdfs:range, rdfs:'Literal').
rdfax0(rdf:value, rdfs:range, rdfs:'Resource').
% [RDFS-13c] RDFS axiomatic triples: subclass hierarchy.
rdfax0(rdf:'Alt', rdfs:subClassOf, rdfs:'Container').
rdfax0(rdf:'Bag', rdfs:subClassOf, rdfs:'Container').
rdfax0(rdf:'Seq', rdfs:subClassOf, rdfs:'Container').
rdfax0(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property').
% [RDFS-13d] RDFS axiomatic triples: subproperty hierarchy.
rdfax0(rdfs:isDefinedBy, rdfs:subPropertyOf, rdfs:seeAlso).
% [RDFS-13e] RDFS axiomatic triples: datatypes.
rdfax0(rdf:'XMLLiteral', rdf:type, rdfs:'Datatype').
rdfax0(rdf:'XMLLiteral', rdfs:subClassOf, rdfs:'Literal').
rdfax0(rdfs:'Datatype', rdfs:subClassOf, rdfs:'Class').
% [RDFS-13f] RDFS axiomatic triples: container membership properies.
rdfax0(rdf:'_1', rdf:type, rdfs:'ContainerMembershipProperty').
rdfax0(rdf:'_1', rdfs:domain, rdfs:'Resource').
rdfax0(rdf:'_1', rdfs:range, rdfs:'Resource').
rdfax0(rdf:'_2', rdf:type, rdfs:'ContainerMembershipProperty').
rdfax0(rdf:'_2', rdfs:domain, rdfs:'Resource').
rdfax0(rdf:'_2', rdfs:range, rdfs:'Resource').
rdfax0(rdf:'_3', rdf:type, rdfs:'ContainerMembershipProperty').
rdfax0(rdf:'_3', rdfs:domain, rdfs:'Resource').
rdfax0(rdf:'_3', rdfs:range, rdfs:'Resource').
/*
rdfax(UriRef, rdf:type, rdfs:'ContainerMembershipProperty'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
rdfax(UriRef, rdfs:domain, rdfs:'Resource'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
rdfax(UriRef, rdfs:range, rdfs:'Resource'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
*/
% =|rdf_db:rdf/3|=
rdfax0(S, P, O):-
  rdf(S, P, O).

test_triple(rdfs:'Resource', rdf:type, rdfs:'Class').
test_triple(rdfs:'Class', rdf:type, rdfs:'Class').
test_triple(rdfs:'Literal', rdf:type, rdfs:'Class').
test_triple(rdf:'XMLLiteral', rdf:type, rdfs:'Class').
test_triple(rdfs:'Datatype', rdf:type, rdfs:'Class').
test_triple(rdf:'Seq', rdf:type, rdfs:'Class').
test_triple(rdf:'Bag', rdf:type, rdfs:'Class').
test_triple(rdf:'Alt', rdf:type, rdfs:'Class').
test_triple(rdfs:'Container', rdf:type, rdfs:'Class').
test_triple(rdf:'List', rdf:type, rdfs:'Class').
test_triple(rdfs:'ContainerMembershipProperty', rdf:type, rdfs:'Class').
test_triple(rdf:'Property', rdf:type, rdfs:'Class').
test_triple(rdf:'Statement', rdf:type, rdfs:'Class').
test_triple(rdfs:domain, rdf:type, rdf:'Property').
test_triple(rdfs:range, rdf:type, rdf:'Property').
test_triple(rdfs:subPropertyOf, rdf:type, rdf:'Property').
test_triple(rdfs:subClassOf, rdf:type, rdf:'Property').
test_triple(rdfs:member, rdf:type, rdf:'Property').
test_triple(rdfs:seeAlso, rdf:type, rdf:'Property').
test_triple(rdfs:isDefinedBy, rdf:type, rdf:'Property').
test_triple(rdfs:comment, rdf:type, rdf:'Property').
test_triple(rdfs:label, rdf:type, rdf:'Property').

