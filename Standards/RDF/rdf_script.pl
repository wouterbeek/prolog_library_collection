:- module(
  rdf_script,
  [
    assert_visum/0
  ]
).

/** <module> RDF script

Scripts for asserting RDF graphs that can be used for debugging.

[[rdfs.png]]

@author Wouter Beek
@version 2012/12-2013/02
*/

:- use_module(generic(os_ext)).
:- use_module(graph_theory(graph_export)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_build)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(graphviz)).

:- rdf_register_prefix(ch,  'http://www.wouterbeek.com/prasem/ch.owl#' ).
:- rdf_register_prefix(dbp, 'http://www.wouterbeek.com/prasem/dbp.owl#').
:- rdf_register_prefix(nl,  'http://www.wouterbeek.com/prasem/nl.owl#' ).

assert_visum:-
  Graph = visum,
  
  % Chinese namespace
  rdfs_assert_individual(ch:cityWithAirport, rdfs:'Class',       Graph),
  rdfs_assert_subclass(  ch:cityWithAirport, rdfs:'Resource',    Graph),
  rdfs_assert_individual(ch:capital,         rdfs:'Class',       Graph),
  rdfs_assert_subclass(  ch:capital,         ch:cityWithAirport, Graph),
  rdfs_assert_individual(ch:'Amsterdam',     ch:capital,         Graph),
  rdfs_assert_individual(ch:visumNeeded,     rdfs:'Class',       Graph),
  rdfs_assert_subclass(  ch:visumNeeded,     rdfs:'Resource',    Graph),
  rdfs_assert_individual(ch:europeanCity,    rdfs:'Class',       Graph),
  rdfs_assert_subclass(  ch:europeanCity,    ch:visumNeeded,     Graph),
  rdfs_assert_individual(ch:'Amsterdam',     ch:europeanCity,    Graph),
  
  % Dutch namespace
  rdfs_assert_individual(nl:europeanCity, rdfs:'Class',     Graph),
  rdfs_assert_subclass(  nl:europeanCity, rdfs:'Resources', Graph),
  rdfs_assert_individual(nl:visumFree,    rdfs:'Class',     Graph),
  rdfs_assert_subclass(  nl:visumFree,    nl:europeanCity,  Graph),
  rdfs_assert_individual(nl:'Amsterdam',  nl:europeanCity,  Graph),
  rdfs_assert_individual(nl:capital,      rdfs:'Class',     Graph),
  rdfs_assert_subclass(  nl:capital,      rdfs:'Resource',  Graph),
  rdfs_assert_individual(nl:'Amsterdam',  nl:capital,       Graph),
  
  % Interrelations
  owl_assert_class_equivalence(ch:capital,      nl:capital,     Graph),
  owl_assert_resource_identity(dbp:'Amsterdam', ch:'Amsterdam', Graph),
  owl_assert_resource_identity(dbp:'Amsterdam', nl:'Amsterdam', Graph).

