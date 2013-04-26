:- module(
  oaei,
  [
  ]
).

/** <module> OAEI

Support for the ontology alignment format.

@author Wouter Beek
@version 2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(file_ext)).
:- use_module(generics(print_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).

% Register the namespaces.
:- rdf_register_prefix(align, 'http://knowledgeweb.semanticweb.org/heterogeneity/alignment#').
:- rdf_register_namespaces.

% Assert the used file types.
:- assert_novel(user:prolog_file_type(owl, owl)).

% Assert the file search paths.
:- assert_novel(user:file_search_path(alignment2, data(alignment2))).
:- assert_novel(user:file_search_path(mapping2, alignment2(alignment))).
:- assert_novel(user:file_search_path(ontology2, alignment2(ontology))).
:- assert_novel(user:file_search_path(reference2, alignment2(reference))).



%% ontologies(+Graph:atom, -File1:atom, -File2:atom) is det.
% Returns the files in which the linked ontologies are stored.

ontologies(Graph, File1, File2):-
  ontology(Graph, File1),
  ontology(Graph, File2),
  File1 \== File2,
  !.

ontology(Graph, File):-
  rdf_literal(Ontology, align:location, URI, Graph),
  rdfs_individual_of(Ontology, align:'Ontology'),
  uri_components(
    URI,
    uri_components(_Scheme, _Authority, Path, _Search, _Fragment)
  ),
  file_base_name(Path, Base),
  absolute_file_name(ontology2(Base), File, [access(read)]).

test:-
  absolute_file_name(
    reference2('cmt-conference'),
    ReferenceFile,
    [access(read), file_type(rdf)]
  ),
  load_alignments(ReferenceFile, ReferencePairs),
gtrace,
  print_list(user_output, ReferencePairs).
/*
  % Alignment attempts.
  absolute_file_name(
    data(alignments),
    AlignmentDir,
    [access(read), file_type(directory)]
  ),
  format(atom(RE), '~w/ *.rdf', [AlignmentDir]),
  expand_file_name(RE, AlignmentFiles),
  maplist(go(ReferencePairs), AlignmentFiles, TestResults),
  print_list(user_output, TestResults).

go(ReferencePairs, AlignmentFile, Atom):-
  % File name.
  file_name(AlignmentFile, _AlignmentDirectory, AlignmentName, _Extension),

  % One alignment pairs.
  load_alignments(AlignmentFile, AlignmentPairs),

  % Statistics
  t1_error(ReferencePairs, AlignmentPairs, FalsePositives),
  t2_error(ReferencePairs, AlignmentPairs, FalseNegatives),
  ord_intersect(ReferencePairs, AlignmentPairs, X),
  length(X, TruePositives),

  atomic_list_concat(
    [AlignmentName, TruePositives, FalsePositives, FalseNegatives],
    '\t',
    Atom
  ).
*/

load_alignments(File, Pairs):-
  file_name(File, _Directory, Graph, _Extension),
  rdf_load2(File, 'RDF/XML', Graph),
  findall(
    From/To,
    (
      rdf(BNode, align:entity1, From, Graph),
      rdf(BNode, align:entity2, To, Graph),
      rdf_datatype(BNode, align:measure, float, _Measure, Graph)
    ),
    Pairs
  ).

