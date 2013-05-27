:- module(
  dbnl_secondary,
  [
    dbnl_secondary/3 % +Graph:atom
                     % +Title:uri
                     % +Contents:dom
  ]
).

/** <module> DBNL SECONDARY

Predicates for processing lists of secondary literature,
as they occur in DBNL title pages.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_generic)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_secondary(+Graph:atom, +Title:uri, +Contents:dom) is det.

dbnl_secondary(Graph, Title, Contents):-
  Contents = [AuthorName, element(a, [href=RelativeURI], [TitleName]), Rest],
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  rdf_bnode(BNode),
  rdf_assert(Title, dbnl:secondary, BNode, Graph),
  rdf_assert_datatype(BNode, dbnl:author, string, AuthorName, Graph),
  rdf_assert_datatype(BNode, dbnl:title, string, TitleName, Graph),
  rdf_assert(BNode, dbnl:original_page, AbsoluteURI, Graph),
  process_rest(Graph, BNode, Rest).

% Publication year
process_rest(Graph, BNode, X1):-
  atom_concat('(', X2, X1),
  !,
  sub_atom(X2, 0, 4, X3, Year),
  rdf_assert_datatype(BNode, dbnl:year, gYear, Year, Graph),
  atom_concat(')', X4, X3),
  process_rest(Graph, BNode, X4).
% Publication venue
process_rest(Graph, BNode, X1):-
  atom_concat('In: ', X2, X1),
  atom_until(', ', X2, PublicationVenueName, X3),
  rdf_assert_datatype(
    BNode,
    dbnl:venue,
    string,
    PublicationVenueName,
    Graph
  ),
  process_rest(Graph, BNode, X3).

