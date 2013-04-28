:- module(
  semweb_tests,
  [
    run_tests/0
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

---+ Q&A

Q: What is the intended use of rdf_db:rdf_global_object/2?

Q: What is the intended use of directive (public/1)?

Q: How should option =|base_uri(+URI)|= for =|rdf_load/2|= be used?
   When I add it RDF/XML files load 0 triples!

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

:- assert_novel(user:prolog_file_type(rdf, rdf)).

:- rdf_register_namespace(test, 'http://www.w3.org/2000/10/rdf-tests/rdfcore/testSchema#').



load_manifest:-
  absolute_file_name(
    tests(testSchema),
    TestSchema,
    [access(read), file_type(rdf)]
  ),
  rdf_load2(TestSchema, test_schema),
  absolute_file_name(
    tests('Manifest'),
    TestFile,
    [access(read), file_type(rdf)]
  ),
  rdf_load2(TestFile, test).

run_tests:-
  load_manifest,
  forall(
    (
      rdf_member(
        TestClass,
        [
          test:'MiscellaneousTest',
          test:'NegativeEntailmentTest',
          test:'NegativeParserTest',
          test:'PositiveEntailmentTest',
          test:'PositiveParserTest'
        ]
      ),
      owl_entailment:rdfs_individual_of(Test, TestClass)
    ),
    run_test(Test)
  ).

run_test(Test):-
  % Clean up the graphs that were used in the previous test.
  maplist(rdf_unload_graph, [conclusion,doc,in,out,premise]),

  % If available, print the description to the screen.
  (
    rdf_literal(Test, test:description, Description1, _Graph),
    strip_begin(['\t',' '], Description1, Description2)
  ->
    format(user_output, 'Test description: ~w\n', [Description2])
  ;
    format(user_output, 'No description for test ~w.\n', [Test])
  ),

  % Run the actual test.
  run_test0(Test).

run_test0(Test):-
  rdf_literal(Test, test:status, Status, _Graph),
  memberchk(Status, ['NOT_APPROVED', 'OBSOLETE', 'WITHDRAWN']),
  !,
  format(user_output, '<<<~w ~w>>>', [Status, Test]).
% A miscellaneous test with a single document.
% These tests are required to throw an exception upon attemption to
% load the 'RDF graph'.
run_test0(Test):-
  (
    owl_entailment:rdfs_individual_of(Test, test:'MiscellaneousTest')
  ;
    owl_entailment:rdfs_individual_of(Test, test:'NegativeParserTest')
  ),
  !,
gtrace,
  
  % Load the document.
  (
    rdf(Test, test:document, URI)
  ;
    rdf(Test, test:inputDocument, URI)
  ),
  !,
  uri_to_file(URI, File),
  
  % Throw an exception.
  catch(
    \+ (
      rdf_load2(File, doc),
      fail
    ),
    _Exception,
    true
  ),
  format(user_output, '<<<PASSED>>>\n\n\n', []).
run_test0(Test):-
  owl_entailment:rdfs_individual_of(Test, test:'NegativeEntailmentTest'),
  !,
  true.
/*
  % The premise graph.
  rdf(Test, test:premiseDocument, Premise_URI),
  uri_to_file(Premise_URI, Premise_File),
  %%%%rdf_load2(Premise_File, in, [base_uri(Premise_URI)]),
  rdf_load2(Premise_File, premise),
*/
% A test with an input and an output document.
run_test0(Test):-
  owl_entailment:rdfs_individual_of(Test, test:'PositiveEntailmentTest'),
  !,
  true.
/*
  % The premise graph.
  rdf(Test, test:premiseDocument, Premise_URI),
  uri_to_file(Premise_URI, Premise_File),
  %%%%rdf_load2(Premise_File, in, [base_uri(Premise_URI)]),
  rdf_load2(Premise_File, premise),

  % Run materialization.
  materialize(premise),
  
  % The conclusion graph.
  % This is loaded after materialization (which cannot be restricted to
  % a graph).
  rdf(Test, test:conclusionDocument, Conclusion_URI),
  uri_to_file(Conclusion_URI, Conclusion_File),
  %%%%rdf_load2(Conclusion_File, out, [base_uri(Conclusion_URI)]),
  rdf_load2(Conclusion_File, conclusion),
  
  % The materialized premise graph must be equivalent to the conclusion graph.
  rdf_graph_equivalence(premise, conclusion).
*/
run_test0(Test):-
  owl_entailment:rdfs_individual_of(Test, test:'PositiveParserTest'),
  !,
gtrace,
  
  % The input graph.
  rdf(Test, test:inputDocument, Input_URI),
  uri_to_file(Input_URI, Input_File),
  %%%%rdf_load2(Input_File, in, [base_uri(Input_URI)]),
  rdf_load2(Input_File, in),

  % The output graph.
  rdf(Test, test:outputDocument, Output_URI),
  uri_to_file(Output_URI, Output_File),
  %%%%rdf_load2(Output_File, out, [base_uri(Output_URI)]),
  rdf_load2(Output_File, out),

  % The graphs must be equivalent.
  rdf_graph_equivalence(in, out),
  format(user_output, '<<<PASSED>>>\n\n\n', []).
run_test0(Test):-
  format(user_output, '<<<FAILED ~w>>>\n\n\n', [Test]).

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

