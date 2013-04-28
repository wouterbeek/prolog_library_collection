:- module(
  rdfs_test,
  [
    run_test/0
  ]
).

/** <module> RDFS TEST

---+ MANIFEST HEADER

==
<?xml version="1.0"?>
<rdf:RDF
  xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#"
  xmlns:test="http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#"
>
==

---+ MANIFEST FOOTER

==
</rdf:RDF>
==

@author Wouter Beek
@version 2013/04
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(db_ext)).
:- use_module(generics(list_ext)).
:- use_module(owl(owl_entailment)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdf(rdf_serial)).
:- use_module(standards(xml)).

:- assert_novel(user:file_search_path(tests, standards('Tests'))).
:- assert_novel(user:prolog_file_type(rdf, rdf)).

:- rdf_register_namespace(test, 'http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#').



load_manifest:-
  absolute_file_name(
    tests('Manifest'),
    TestFile,
    [access(read), file_type(rdf)]
  ),
  rdf_load2(TestFile, test).

run_test:-
  load_manifest,
  forall(
    owl_entailment:rdfs_individual_of(Test, test:'PositiveParserTest'),
    run_positive_parser_test(Test)
  ).

run_positive_parser_test(Test):-
  (
    rdf_literal(Test, test:description, Description1, _Graph),
    strip_begin(['\t',' '], Description1, Description2)
  ->
    format(user_output, 'Test description: ~w\n', [Description2])
  ;
    format(user_output, 'No description for test ~w.\n', [Test])
  ),
  rdf(Test, test:inputDocument, Input_URI),
  uri_to_file(Input_URI, Input_File),
  rdf_load2(Input_File, in),
  rdf(Test, test:outputDocument, Output_URI),
  uri_to_file(Output_URI, Output_File),
  rdf_load2(Output_File, out),
  rdf_graph_equivalence(in, out).

%% uri_to_file(+URI:uri, -File:atom) is det.
% Returns the atomic name of the locally stored file that the given URI
% refers to.
% This assumes that the test files are stored locally.

uri_to_file(URI, File):-
  uri_components(
    URI,
    uri_components(http, 'www.w3.org', URI_Path, _Search, _Fragment)
  ),
  split_atom_exclusive(URI_Path, '/', List),
  nth_minus_0(0, List, FileName),
  nth_minus_0(1, List, Directory),
  absolute_file_name(
    tests(Directory),
    SubDirectory,
    [access(read), file_type(directory)]
  ),
  absolute_file_name(
    FileName,
    File,
    [access(read), relative_to(SubDirectory)]
  ).

