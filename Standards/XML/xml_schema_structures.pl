:- module(
  xml_schema,
  [
  ]
).

/** <module> XML SCHEMA

XML Schema Definition Language, offering facilities for describing the
structure and constraining the contents of XML documents.

Substantially reconstructs and considerable extends the capabilities of DTD.

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).

:- xml_register_namespace(w3c, 'http://www.w3.org/').

init:-
  Graph = w3c,
  rdf_global_id(w3c:'TR/2012/REC-xmlschema11-1-20120405/', This),
  rdfs_assert_individual(This, w3c:'Recommendation', Graph),
  rdf_assert_datatype(This, w3c:year, gYear, 2012, Graph),
  rdf_assert_literal(
    This,
    std:title,
    'XML Schema Definition Language (XSD) 1.1 Part 1: Structures',
    Graph
  ),
  rdf_assert_literal(This, w3c:author, 'Shudi (Sandy) Gao 高殊镝', Graph),
  rdf_assert_literal(This, w3c:author, 'C. M. Sperberg-McQueen', Graph),
  rdf_assert_literal(This, w3c:author, 'Henry S. Thompson', Graph),
  % XML Schema Definition Language 1.1 Part 2: Datatypes
  rdf_assert(This, w3c:depends_on, w3c:'???', Graph), 
  true.
:- init.

