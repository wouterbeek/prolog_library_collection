:- module(
  rdf_entails,
  [
    rdf_entails/5, % ?S:uri
                   % ?P:uri
                   % ?O:uri
                   % ?G:atom
                   % -Proof:compound
    rdf_entails/7, % ?S:uri
                   % ?P:uri
                   % ?O:uri
                   % ?G:atom
                   % +MaxD:integer
                   % -D:integer
                   % -Proof:compound
    rdf_entails_dev/4, % ?S:uri
                       % ?P:uri
                       % ?O:uri
                       % ?G:atom
    rdf_entails_dev/5 % ?S:uri
                      % ?P:uri
                      % ?O:uri
                      % ?G:atom
                      % +MaxD:integer
  ]
).

/** <module> RDF entails

Entailment regime for RDF(S) and OWL.

@author Wouter Beek
@version 2013/01
*/

:- use_module(generics(print_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdfs(rdfs_read)).

:- rdf_meta(rdf_entails(r,r,r,?,-)).
:- rdf_meta(rdf_entails(r,r,r,?,+,-,-)).
:- rdf_meta(rdf_entails0(r,r,r,?,+,+,-,-)).
:- rdf_meta(rdf_entails_dev(r,r,r,?)).
:- rdf_meta(rdf_entails_dev(r,r,r,?,+)).



%! rdf_entails(?S:uri, ?P:uri, ?O:uri, ?G:atom, -Proof:compound) is nondet.
% @see rdf_entails/7

rdf_entails(S, P, O, G, Proof):-
  rdf_entails(S, P, O, G, infinite, _D, Proof).

%! rdf_entails(?S:uri, ?P:uri, ?O:uri, ?G:atom, +MaxD:integer,
%!   -D:integer, -Proof:compound
%! ) is nondet.
%
% @arg S The subject resource.
% @arg P The predicate resource.
% @arg O The object resource.
% @arg G The atomic name of a graph.
% @arg MaxD An integer indicating the maximum depth at which predicates
%        are searched to fit into the proof.
% @arg D An integer indicating the highest depth at which predicates were
%        search for the proof.
% @arg Proof A compound term of the form =|proof(Conclusion, Predicates)|=
%        representing a proof.

rdf_entails(S, P, O, G, MaxD, D, Proof):-
  (
    MaxD == infinite,
    !
  ;
    must_be(positive_integer, MaxD)
  ),
  var(D),
  !,
  rdf_entails0(S, P, O, G, MaxD, 0, D, Proof).

%! rdf_entails0(S, P, O, G, MaxD, MaxD, _D, _Proof) is nondet.
% Keeps track of the depth of the traversed proof space, ensuring this does not
% exceed the maximum depth.
%
% @arg S The subject resource.
% @arg P The predicate resource.
% @arg O The object resource.
% @arg G The atomic name of a graph.
% @arg MaxD An integer indicating the maximum depth at which predicates
%        are searched to fit into the proof.
% @arg D0 Counter, starting at 0, should not reach =MaxD=.
% @arg D The detph of =Proof=.
% @arg Proof A compound term of the form =|proof(Conclusion, Predicates)|=
%        representing a proof.

% The maximum depth has been reached.
rdf_entails0(_S, _P, _O, _G, MaxD, MaxD, _D, _Proof):-
  !,
  fail.
% Basic relation.
rdf_entails0(S, P, O, G, _MaxD, D, D, proof(rdf(S, P, O), [])):-
  rdf(S, P, O, G),
  rdf_is_resource(O).
% Instance-of relation.
rdf_entails0(S, Type, C, G, MaxD, D0,
  D, proof(rdf(S, Type, C), [proof(rdf(S, Type, C0), []), Proofs])
):-
  rdf_global_id(rdf:type, Type),
  rdfs_individual_of(S, C0),
  succ(D0, D1),
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdf_entails0(C0, SubClassOf, C, G, MaxD, D1, D, Proofs).
% Subclass relation.
rdf_entails0(S, SubClassOf, O, G, MaxD, D0,
  D, proof(rdf(S, SubClassOf, O), [proof(rdf(S, SubClassOf, S0), []), Proofs])
):-
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdfs_subclass_of(S, S0),
  succ(D0, D1),
  rdf_entails0(S0, SubClassOf, O, G, MaxD, D1, D, Proofs).
/*
% Instance-of relation.
rdf_entails0(S, R, C0, G, MaxD, D0,
  D, proof(rdf(S, R, C0), [proof(rdf(S, R, C), []), Proofs])
):-
  rdf_global_id(rdf:type, R),
  rdfs_individual_of(S, C),
  succ(D0, D1),
  rdf_entails0(C, rdfs:subClassOf, C0, G, MaxD, D1, D, Proofs).
% Subproperty relation.
rdf_entails0(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(P, SubPropertyOf, P0), []), Proofs])
):-
  rdf_global_id(rdfs:subPropertyOf, SubPropertyOf),
  rdfs_subproperty_of(P, P0),
  succ(D0, D1),
  rdf_entails0(S, P0, O, G, MaxD, D1, D, Proofs).
% Subclass relation.
rdf_entails0(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(S, SubClassOf, S0), []), Proofs])
):-
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdfs_subclass_of(S, S0),
  succ(D0, D1),
  rdf_entails0(S0, P, O, G, MaxD, D1, D, Proofs).
rdf_entails0(S, P, O, _G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(O, SubClassOf, O0), []), Proofs])
):-
  rdf_global_id(rdfs:subClassOf, SubClassOf),
  rdfs_subclass_of(O, O0),
  succ(D0, D1),
  rdf_entails0(S, P, O0, G, MaxD, D1, D, Proofs).
% Identity relation.
rdf_entails0(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(S, SameAs, S0), []), Proofs])
):-
  rdf_global_id(owl:sameAs, SameAs),
  owl_resource_identity(S, S0),
  succ(D0, D1),
  rdf_entails0(S0, P, O, G, MaxD, D1, D, Proofs).
rdf_entails0(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(O, SameAs, O0), []), Proofs])
):-
  rdf_global_id(owl:sameAs, SameAs),
  owl_resource_identity(O, O0),
  succ(D0, D1),
  rdf_entails0(S, P, O0, G, MaxD, D1, D, Proofs).
% Equivalence relation.
rdf_entails0(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(S, EquivalentClass, S0), []), Proofs])
):-
  rdf_global_id(owl:equivalentClass, EquivalentClass),
  owl_class_equivalence(S, S0),
  succ(D0, D1),
  rdf_entails0(S0, P, O, G, MaxD, D1, D, Proofs).
rdf_entails0(S, P, O, G, MaxD, D0,
  D, proof(rdf(S, P, O), [proof(rdf(O, EquivalentClass, O0), []), Proofs])
):-
  rdf_global_id(owl:equivalentClass, EquivalentClass),
  owl_class_equivalence(O, O0),
  succ(D0, D1),
  rdf_entails0(S, P, O0, G, MaxD, D1, D, Proofs).
*/

%! rdf_entails_dev(?S:uri, ?P:uri, ?O:uri, ?G:atom) is nondet.
% @see rdf_entails/7

rdf_entails_dev(S, P, O, G):-
  rdf_entails_dev(S, P, O, G, infinite).

%! rdf_entails_dev(?S:uri, ?P:uri, ?O:uri, ?G:atom, +MaxD:integer) is nondet.
% @see rdf_entails/7

rdf_entails_dev(S, P, O, G, MaxD):-
  rdf_entails(S, P, O, G, MaxD, D, Proof),
  debug(proof, '~w', [Proof]),
  print_ext:print_proof(user, [depth(D), indent(0), max_depth(MaxD)], [Proof]).
