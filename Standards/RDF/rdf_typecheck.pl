:- module(
  rdf_typecheck,
  [
    rdf_is_subject/1,
    rdf_is_predicate/1,
    rdf_is_object/1
  ]
).

/** <module> RDF TYPECHECK

Type checking extension for RDF.

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(typecheck)).
:- use_module(library(semweb/rdf_db)).

:- rdf_meta(rdf_is_object(r)).
:- rdf_meta(rdf_is_predicate(r)).
:- rdf_meta(rdf_is_subject(r)).



rdf_is_object(Object):-
  rdf_is_subject(Object).
rdf_is_object(Object):-
  rdf_is_literal(Object).

rdf_is_predicate(Predicate):-
  is_uri(Predicate).

rdf_is_subject(Subject):-
  rdf_is_bnode(Subject).
rdf_is_subject(Subject):-
  is_uri(Subject).
