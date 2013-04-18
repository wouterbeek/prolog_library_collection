module(
  identity,
  [
    load_data/0
  ]
).

/** <module> IDENTITY ON THE WEB

My first publication with Stephan!

@author Wouter Beek
@version 2013/04
*/

:- use_module(datasets(dbpedia)).
:- use_module(generics(meta_ext)).
:- use_module(generics(print_ext)).
:- use_module(generics(thread_ext)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(owl(owl_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_namespace)).
:- use_module(sparql(sparql_ext)).



% DATA %

load_data:-
  absolute_file_name(pgc(persons), File, [access(read), file_type(turtle)]),
  rdf_load2(File, iotw).

load_niod_data:-
  absolute_file_name(pgc(entities), Entities, [access(read), file_type(turtle)]),
  rdf_load2(Entities, niod),
  absolute_file_name(pgc(ner_all_to_dbp_nl), NIOD_Linkset, [access(read), file_type(turtle)]),
  rdf_load2(NIOD_Linkset, niod_dbpedia),
  absolute_file_name(pgc(dbpedia_dump), DBpedia, [access(read), file_type(turtle)]),
  rdf_load2(DBpedia, dbpedia).

save_data:-
  rdf_register_namespaces,
  forall(
    rdf_graph(Graph),
    rdf_save2(Graph)
  ).



% PROPERTY PATHS %

compare_property_paths(X, OnlyX, Y, OnlyY, Shared):-
  setoff(
    PropertyPathX,
    property_path(X, PropertyPathX),
    PropertyPathsX
  ),
  setoff(
    PropertyPathY,
    property_path(Y, PropertyPathY),
    PropertyPathsY
  ),
  ord_intersect(PropertyPathsX, PropertyPathsY, Shared),
  ord_subtract(PropertyPathsX, Shared, OnlyX),
  ord_subtract(PropertyPathsY, Shared, OnlyY).

% Leibniz' Law.
% The principle of the indiscernability of identicals.
leibniz_law(X, Y):-
  owl_resource_identity(X, Y),
  forall(
    property_path(X, PropertyPath),
    property_path(Y, PropertyPath)
  ),
  forall(
    property_path(Y, PropertyPath),
    property_path(X, PropertyPath)
  ).

property_path(Subject, [Property | Properties]):-
  rdf(Subject, Property, Object),
  rdf_is_bnode(Object),
  property_path(Object, Properties).
property_path(Subject, [Property-Object]):-
  rdf(Subject, Property, Object),
  rdf_is_literal(Object).
property_path(Subject, [Property-Object]):-
  rdf(Subject, Property, Object),
  rdf_is_resource(Object).

shared_property_paths(X, Y, Shared):-
  compare_property_paths(X, _OnlyX, Y, _OnlyY, Shared).



% PROPERTIES %

% Discernability
shared_property(X, Y, [Property, [ValueX, ValueY]]):-
  rdf(X, Property, ValueX),
  \+ (rdf(X, Property, ValueXX), ValueXX \== ValueX),
  rdf(Y, Property, ValueY),
  ValueX \== ValueY.

% Indiscernability
shared_property_value(X, Y, [Property, [Value]]):-
  rdf(X, Property, Value),
  rdf(Y, Property, Value).



% SCRIPTS %

test1:-
  rdf_global_id(dbpedia:'Izaak_H._Reijnders', X),
  assert_resource(X, iotw),
  rdf_global_id(dbpedia:'Didier_Reynders', Y),
  assert_resource(Y, iotw),

  format(user, 'SHARED PROPERTIES\n', []),
  setoff(P, shared_property(X, Y, P), Ps),
  print_list(user, Ps),

  format(user, 'SHARED PROPERTY VALUES\n', []),
  setoff(PV, shared_property_value(X, Y, PV), PVs),
  print_list(user, PVs).

test2:-
  formulate_sparql(
    [],
    'SELECT DISTINCT ?s',
    ['  ?s a foaf:Person .'],
    0,
    Query
  ),
  enqueue_sparql(dbpedia, Query, _VarNames, Rows),
  run_on_sublists(Rows, identity:assert_persons).

assert_persons(List):-
  maplist(assert_person, List).

assert_person(row(Person)):-
  thread_self(SelfId),
  assert_resource(Person, SelfId),
  thread_success(SelfId).

