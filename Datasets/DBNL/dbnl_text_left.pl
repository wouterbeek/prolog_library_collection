:- module(
  dbnl_text_left,
  [
    dbnl_text_left/3 % +Graph:atom
                     % +Text:uri
                     % +Contents:dom
  ]
).

/** <module> DBNL TEXT LEFT

Predicates for scraping the left DIV of text pages in the DBNL.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(generics(atom_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



% Done!
dbnl_text_left(_Graph, _Text, []):-
  !.
% Author.
dbnl_text_left(
  Graph,
  Text,
  [AuthorAtom, element(a, Attributes, [AuthorName]) | T]
):-
  strip([' ',':'], AuthorAtom, 'auteur'),
  !,
  memberchk(href=RelativeURI, Attributes),
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  (
    rdf_retractall(Text, dbnl:supposed_author, AuthorName, Graph)
  ->
    dbnl_assert_author(Graph, AbsoluteURI, AuthorName, Author),
    rdf_assert(Text, dbnl:author, Author, Graph)
  ;
    rdf_retractall(Text, dbnl:supposed_editor, AuthorName, Graph)
  ->
    dbnl_assert_editor(Graph, AbsoluteURI, AuthorName, Editor),
    rdf_assert(Text, dbnl:editor, Editor, Graph)
  ;
    gtrace %DEB
  ),
  dbnl_text_left(Graph, Text, T).
% Illustrator.
dbnl_text_left(
  Graph,
  Text,
  [IllustratorAtom, element(a, Attributes, [IllustratorName]) | T]
):-
  strip([' ',':'], IllustratorAtom, 'illustrator'),
  !,
  memberchk(href=RelativeURI, Attributes),
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  dbnl_assert_author(Graph, AbsoluteURI, IllustratorName, Illustrator),
  rdf_assert(Text, dbnl:illustrator, Illustrator, Graph),
  dbnl_text_left(Graph, Text, T).
% Skip title.
dbnl_text_left(
  Graph,
  Text,
  [element(h3, [], [element(a, _, [element(h3, [], _)])]) | T]
):-
  !,
  dbnl_text_left(Graph, Text, T).
% Source.
dbnl_text_left(
  Graph,
  Text,
  [AuthorName1, element(i, [], [PublicationName]) | T]
):-
  !,
  atom_concat('bron: ', AuthorName2, AuthorName1),
  % Just checking...
  (
    rdf(Text, dbnl:author, Author, Graph),
    rdfs_label(Author, AuthorName2),
    rdfs_label(Text, PublicationName)
  ->
    true
  ;
    gtrace %DEB
  ),
  dbnl_text_left(Graph, Text, T).
% Parse the content of paragraphs.
dbnl_text_left(Graph, Text, [element(p, [], H) | T]):-
  !,
  dbnl_text_left(Graph, Text, H),
  dbnl_text_left(Graph, Text, T).
% Parse the content of DIVs.
dbnl_text_left(Graph, Text, [element(div, [], H) | T]):-
  !,
  dbnl_text_left(Graph, Text, H),
  dbnl_text_left(Graph, Text, T).
% Skip line breaks.
dbnl_text_left(Graph, Text, [element(br, _, []) | T]):-
  !,
  dbnl_text_left(Graph, Text, T).
% Copyright.
dbnl_text_left(Graph, Text, [Atom | Contents]):-
  dbnl_copyright(Graph, Text, Atom),
  !,
  dbnl_text_left(Graph, Text, Contents).
% Debug.
dbnl_text_left(Graph, Text, [H | T]):-
  gtrace, %DEB
  format(user_output, '~w\n', [H]),
  dbnl_text_left(Graph, Text, T).

