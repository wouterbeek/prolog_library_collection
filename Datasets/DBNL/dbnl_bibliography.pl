:- module(
  dbnl_bibliography,
  [
    dbnl_bibliography/2 % +Options:list(nvpair)
                        % +URI:uri
  ]
).

/** <module> DBNL BIBLIOGRAPHY

Predicates for parsing DBNL bibliography texts.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_markup)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(xpath)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_bibliography(+Options:list(nvpair), +URI:uri) is det.

dbnl_bibliography(Options, URI):-
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents),
  xpath_chk(Contents, //p(content), Content),
  split_list_exclusive(
    Content,
    [element(br, _, []), element(br, _, [])],
    Chunks
  ),
  maplist(dbnl_bibliography0(Options), Chunks).

dbnl_bibliography0(Options, Chunk1):-
  dbnl_markup(Options, Chunk1, Chunk2),
  dom_to_xml(dbnl, Chunk2, XML),
  option(graph(Graph), Options),
  option(title(Title), Options),
  rdf_bnode(BNode),
  rdfs_assert_individual(BNode, dbnl:'Publication', Graph),
  rdf_assert(Title, dbnl:bibliography, BNode, Graph),
  rdf_assert_xml_literal(BNode, dbnl:unprocessed, XML, Graph).

/* TOO DIFFICULT FOR NOW!
dbnl_bibliography(Graph, Title, BNode, [Year1 | Contents]):-
  atom(Year1),
  dbnl_extract_year(Year1, Year2),
  !,
  rdf_assert_datatype(BNode, dbnl:year, gYear, Year2, Graph),
  dbnl_bibliography(Graph, Title, BNode, Contents).
dbnl_bibliography(Graph, Title, BNode, [Author1 | Contents]):-
  atom(Author1),
  dbnl_extract_author(Author1, Author2),
  !,
  rdf_assert_datatype(BNode, dbnl:author, string, Author2, Graph),
  dbnl_bibliography(Graph, Title, BNode, Contents).
*/

