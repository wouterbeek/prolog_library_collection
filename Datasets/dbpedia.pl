:- module(
  dbpedia,
  [
    assert_identity_resource/2, % +Resource:uri
                                % +Graph:atom
    assert_resource/2, % +Resource:uri
                       % +Graph:atom
    describe_resource/2, % +Resource:uri
                         % -Rows:list(row)
    find_dbpedia_agent/4, % +Name:atom
                          % +Birth:integer
                          % +Death:integer
                          % -DBpediaAgent:uri
    link_to_dbpedia_agents/1 % +Graph:atom
  ]
).

/** <module> DBpedia

Querying DBpedia using SPARQL.

---+ Examples

First query for a resource:

==
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX umbel: <http://umbel.org/umbel/rc/>
SELECT DISTINCT ?s
WHERE {
  ?s rdf:type umbel:Writer .
  ?s rdfs:label ?label .
  FILTER regex(?label, "Queneau", "i")
}
LIMIT 1
OFFSET 0
==

Then retrieve all known facts about the result:

==
PREFIX dbpedia: <http://dbpedia.org/resource/>
SELECT ?p ?o
WHERE
{
  dbpedia.org:Raymond_Queneau ?p ?o .
}
==

---+ tmp

    <instance_types_en.ttl> ,
    <mappingbased_properties_en.ttl> ,
    <specific_mappingbased_properties_en.ttl> .

@author Wouter Beek
@version 2013/03-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdfs)).
:- use_module(owl(owl_build)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).
:- use_module(sparql(sparql_ext)).

:- assert_novel(user:prolog_file_type(ttl, turtle)).

:- rdf_register_namespace(dbpedia).
:- rdf_register_namespace('dbpedia-owl').
:- rdf_register_namespace(dbpprop).
:- rdf_register_namespace(foaf).
:- rdf_register_namespace('powder-s').
:- rdf_register_namespace(stcn, 'http://stcn.data2semantics.org/resource/').
:- rdf_register_namespace(umbel).
:- rdf_register_namespace(yago).

:- register_sparql_prefix(dbpedia).
:- register_sparql_prefix('dbpedia-owl').
:- register_sparql_prefix(dbpprop).
:- register_sparql_prefix(umbel).
:- register_sparql_prefix(yago).

:- rdf_meta(assert_resource(r,+)).
:- rdf_meta(describe_resource(r,-)).
:- rdf_meta(find_dbpedia_agent(+,+,+,r)).
:- rdf_meta(link_to_dbpedia_agent(+,r)).

:- register_sparql_remote(dbpedia, 'dbpedia.org', default, '/sparql').

:- debug(dbpedia).



assert_identity_resource(FromSubject, Graph):-
  setoff(
    ToSubject,
    owl_resource_identity(FromSubject, ToSubject),
    ToSubjects
  ),
  forall(
    member(Subject, [FromSubject | ToSubjects]),
    (
      assert_resource(Subject, Graph)
    )
  ).

assert_resource(Subject, Graph):-
  rdf_is_resource(Subject),
  atom(Graph),
  describe_resource(Subject, Rows),
  forall(
    member(row(Predicate, Object), Rows),
    rdf_assert(Subject, Predicate, Object, Graph)
  ).

describe_resource(Resource, Rows):-
  format(atom(Where), '  <~w> ?p ?o .', [Resource]),
  formulate_sparql(
    [],
    'SELECT DISTINCT ?p ?o',
    [Where],
    0,
    Query
  ),
  enqueue_sparql(dbpedia, Query, _VarNames, Rows),
  if_then(
    Rows == [],
    debug(dbpedia, 'Empty results for DESCRIBE ~w.', [Resource])
  ).

%% find_person(
%%   +FullName:atom,
%%   +Birth:integer,
%%   +Death:integer,
%%   -DBpediaAuthor:uri
%% ) is semidet.

find_dbpedia_agent(Name, Birth, Death, DBpediaAuthor):-
  format(
    atom(Where),
    [
      '?writer rdf:type foaf:Person .',
      '?writer rdfs:label ?label .',
      'FILTER regex(?label, "~w", "i")',
      '?writer dbpprop:dateOfBirth ?birth .',
      'FILTER regex(?birth, "~w")',
      '?writer dbpprop:dateOfDeath ?death .',
      'FILTER regex(?death, "~w")'
    ],
    [Name, Birth, Death]
  ),
  formulate_sparql(
    [dbpprop, foaf],
    'SELECT DISTINCT ?writer',
    Where,
    10,
    Query
  ),
  enqueue_sparql(dbpedia, Query, _VarNames, Resources),
  (
    Resources = []
  ->
    debug(dbpedia, 'Could not find a resource for \'~w\'.', [Name])
  ;
    first(Resources, row(DBpediaAuthor))
  ).

link_to_dbpedia_agents(Graph):-
  setoff(
    Agent,
    (
      rdfs_individual_of(Agent, foaf:'Agent'),
      rdf_subject(Graph, Agent)
    ),
    Agents
  ),
  run_on_sublists(Agents, link_to_dbpedia_agents(Graph)).

link_to_dbpedia_agents(Graph, Agents):-
  maplist(link_to_dbpedia_agent(Graph), Agents).

link_to_dbpedia_agent(Graph, Agent):-
  rdf_datatype(Agent, foaf:name, string, Name, Graph),
  rdf_datatype(Agent, stcn:birth, gYear, Birth, Graph),
  rdf_datatype(Agent, stcn:death, gYear, Death, Graph),
  find_dbpedia_agent(Name, Birth, Death, DBpediaAgent),
  owl_assert_resource_identity(Agent, DBpediaAgent, Graph),
  debug(dbpedia, 'Agent ~w linked to DBpedia agent ~w.', [Agent, DBpediaAgent]).

/*
load:-
  assert(user:file_search_path(data_dbpedia, data('DBpedia'))),
  % Check for existence and read access.
  absolute_file_name(
    data('DBpedia'),
    _Directory,
    [access(read), file_type(directory)]
  ),
  absolute_file_name(
    data_dbpedia(void),
    File,
    [access(read), file_type(turtle)]
  ),
  rdf_attach_library(File),
  rdf_load_library('dbpedia-owl'),
  rdf_load_library(dbpedia).
*/

