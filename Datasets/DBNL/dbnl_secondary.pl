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
:- use_module(dbnl(dbnl_text)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_secondary(+Graph:atom, +Title:uri, +Contents:dom) is det.

dbnl_secondary(
  Graph,
  Title,
  [
    AuthorName,
    element(a, [href=RelativeURI], [TitleName]),
    Atom
  ]
):-
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  dbnl_text(Graph, Title, AbsoluteURI, SecondaryText),
  rdf_assert(Title, dbnl:secondary, SecondaryText, Graph),
  rdf_assert_datatype(SecondaryText, dbnl:author, string, AuthorName, Graph),
  rdf_assert_datatype(SecondaryText, dbnl:title, string, TitleName, Graph),
  rdf_assert(SecondaryText, dbnl:original_page, AbsoluteURI, Graph),
  dbnl_secondary0(Graph, Title, SecondaryText, Atom).

% Year.
dbnl_secondary0(Graph, Title, SecondaryText, Atom1):-
  atom_concat('(', Atom2, Atom1),
  sub_atom(Atom2, 0, 4, Atom3, Year),
  rdf_assert_datatype(SecondaryText, dbnl:year, gYear, Year, Graph),
  atom_concat(')', Atom4, Atom3),
  !,
  dbnl_secondary0(Graph, Title, SecondaryText, Atom4).
% Journal.
dbnl_secondary0(Graph, Title, SecondaryText, Atom1):-
  atom_concat('In: ', Atom2, Atom1),
  atom_until(', ', Atom2, PublicationVenueName, Atom3),
  !,
  rdf_assert_datatype(
    SecondaryText,
    dbnl:venue,
    string,
    PublicationVenueName,
    Graph
  ),
  dbnl_secondary0(Graph, Title, SecondaryText, Atom3).
% Done?
dbnl_secondary0(_Graph, _Title, _SecondaryText, Atom):-
  gtrace, %DEB
  format(user_output, '~w\n', [Atom]).

