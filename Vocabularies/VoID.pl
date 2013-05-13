:- module(
  void,
  [
% RDF LINKS & LINKSETS
    rdf_link/5, % ?Subject:oneof([bnode,uri])
                % ?Predicate:uri
                % ?Object:oneof([bnode,literal,uri])
                % ?FromGraph:atom
                % ?ToGraph:atom
    rdf_linkset/3, % ?Triples:list(rdf_triples)
                   % ?FromGraph:atom
                   % ?ToGraph:atom

% VOID DATASETS
    void_assert_statistics/3, % +Dataset:uri
                              % +DatasetGraph:atom
                              % +VoID_Graph:atom
    void_load_library/2, % +VoID_File:atom
                         % -VoID_Graph:atom
    void_save_library/1, % +VoID_Graph:atom
    void_update_library/1 % +VoID_Graph:atom
  ]
).

/** <module> VoID

Support for the Vocabulary of Interlinked Datasets (VoID).

VoID is a W3C Interest Group Note as of 2011/03/03.

@author WouterBeek
@compat http://www.w3.org/TR/void/
@version 2013/03-2013/05
*/

:- use_module(library(debug)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_serial)).
:- use_module(rdf(rdf_statistics)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(void, 'http://rdfs.org/ns/void#').

:- dynamic(dataset(_VoID_Graph, _Dataset, _DatasetPath, _DatasetGraph)).

:- debug(void).



load_dataset(Dataset):-
  once(rdf(Dataset, void:dataDump, FileName)),
  absolute_file_name(data(FileName), File, [access(read)]),
  rdf_global_id(_Namespace:LocalName, Dataset),
  rdf_load(File, [graph(LocalName)]).

%% rdf_link(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Object:oneof([bnode,literal,uri]),
%%   ?FromGraph:atom,
%%   ?ToGraph:atom
%% ) is nondet.
% An RDF link is an RDF triple whose subject and object are described in
% different datasets.

rdf_link(Subject, Predicate, Object, FromGraph, ToGraph):-
  rdf(Subject, Predicate1, Object1, FromGraph),
  \+ ((
    Predicate1 == Predicate,
    Object1 == Object
  )),
  rdf(Object, Predicate2, Object2, ToGraph),
  \+ ((
    Predicate2 == Predicate,
    Object2 == Object
  )),
  FromGraph \== ToGraph.

%% rdf_linkset(+Triples:list(rdf_triple), ?FromGraph:atom, ?ToGraph:atom) is semidet.
%% rdf_linkset(-Triples:list(rdf_triple), +FromGraph:atom, +ToGraph:atom) is det
% An RDF linkset is a collection of RDF links between the same two datasets.

rdf_linkset(Triples, FromGraph, ToGraph):-
  nonvar(Triples),
  !,
  forall(
    member(row(Subject, Predicate, Object), Triples),
    rdf_link(Subject, Predicate, Object, FromGraph, ToGraph)
  ).
rdf_linkset(Triples, FromGraph, ToGraph):-
  findall(
    row(Subject, Predicate, Object),
    rdf_link(Subject, Predicate, Object, FromGraph, ToGraph),
    Triples
  ).

void_assert_modified(Dataset, DatasetPath, VoID_Graph):-
  time_file(DatasetPath, LastModified),
  rdf_overwrite_datatype(
    Dataset,
    dc:modified,
    dateTime,
    LastModified,
    VoID_Graph
  ).

void_assert_statistics(Dataset, DatasetGraph, VoID_Graph):-
  % void:classes
  count_classes(DatasetGraph, NumberOfClasses),
  rdf_overwrite_datatype(
    Dataset,
    void:classes,
    int,
    NumberOfClasses,
    VoID_Graph
  ),

  % void:distinctObjects
  count_objects(_Subject10, _Predicate10, DatasetGraph, NumberOfObjects),
  rdf_overwrite_datatype(
    Dataset,
    void:distinctObjects,
    int,
    NumberOfObjects,
    VoID_Graph
  ),

  % void:distinctSubjects
  count_subjects(_Predicate20, _Object20, DatasetGraph, NumberOfSubjects),
  rdf_overwrite_datatype(
    Dataset,
    void:distinctSubjects,
    int,
    NumberOfSubjects,
    VoID_Graph
  ),

  % void:properties
  count_properties(DatasetGraph, NumberOfProperties),
  rdf_overwrite_datatype(
    Dataset,
    void:properties,
    int,
    NumberOfProperties,
    VoID_Graph
  ),

  % void:triples
  rdf_statistics(triples_by_graph(DatasetGraph, NumberOfTriples)),
  rdf_overwrite_datatype(
    Dataset,
    void:triples,
    int,
    NumberOfTriples,
    VoID_Graph
  ).

%% void_load_library(+VoID_File:atom, -VoID_Graph:atom) is det.
% Loads a VoID file and all the datasets defined in it.
% Also calculates VoID statistics for all datasets and asserts those to
% the VoID file.
%
% @tbd Use threads for loading the datasets.

void_load_library(VoID_File, VoID_Graph):-
  nonvar(VoID_File),
  exists_file(VoID_File),
  access_file(VoID_File, read),
  !,
  retractall(dataset(_VoID_Graph, _Dataset, _DatasetPath, _DatasetGraph)),
  file_directory_name(VoID_File, VoID_Directory),
  rdf_load2(VoID_File, VoID_Graph),
  VoID_Graph \== void_schema,
  absolute_file_name(
    vocabularies('VoID'),
    VoidSchemaFile,
    [access(read), file_type(turtle)]
  ),
  rdf_load2(VoidSchemaFile, void_schema),
  debug(void, 'VoID file ~w loaded.', [VoidSchemaFile]),
  findall(
    ThreadId,
    (
      % This includes VoID linksets.
      rdfs_individual_of(Dataset, void:'Dataset'),
      rdf(Dataset, void:dataDump, DataDump, VoID_Graph),
      directory_file_path(VoID_Directory, DataDump, DatasetPath),
      thread_create(
        (
          rdf_load2(DatasetPath, DatasetGraph),
          assert(dataset(VoID_Graph, Dataset, DatasetPath, DatasetGraph))
        ),
        ThreadId,
        [detached(false)]
      )
    ),
    ThreadIds
  ),
  forall(
    member(ThreadId, ThreadIds),
    thread_join(ThreadId, true)
  ),
  void_update_library(VoID_Graph),
  rdf_save2(VoID_Graph, VoID_File).
void_load_library(Stream, LocalName):-
  rdf_guess_data_format(Stream, Format),
  rdf_load(Stream, [format(Format), graph(temp_void)]),
  once(rdfs_individual_of(DatasetDescription, void:'DatasetDescription')),
  rdf_global_id(_Namespace:LocalName, DatasetDescription),
  rdf_graph_merge(temp_void, LocalName),
  rdf_unload_graph(temp_void),
  forall(
    rdfs_individual_of(Dataset, void:'Dataset'),
    load_dataset(Dataset)
  ).

void_save_library(VoID_Graph):-
  findall(
    ThreadId,
    (
      dataset(VoID_Graph, _Dataset, DatasetPath, DatasetGraph),
      thread_create(
        rdf_save2(DatasetPath, [format(turtle), graph(DatasetGraph)]),
        ThreadId,
        [detached(false)]
      )
    ),
    ThreadIds
  ),
  forall(
    member(ThreadId, ThreadIds),
    thread_join(ThreadId, true)
  ),
  rdf_save2(_VoID_File, [format(turtle), graph(VoID_Graph)]).

void_update_library(VoID_Graph):-
  set_prolog_stack(global, limit(2*10**9)),
  forall(
    dataset(VoID_Graph, Dataset, DatasetPath, DatasetGraph),
    (
      void_assert_modified(Dataset, DatasetPath, VoID_Graph),
      void_assert_statistics(Dataset, DatasetGraph, VoID_Graph),
      format(user, 'Updated dataset ~w.\n', [Dataset])
    )
  ).

