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
:- use_module(dbnl(dbnl_text)).
:- use_module(generics(atom_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_text_left(+Graph:atom, +Text:uri, +HTML:dom) is det.
% @tbd Parse the content of 'Met illustraties van ...'

% Done!
dbnl_text_left(_Graph, _Text, []):-
  !.
% Skip title.
dbnl_text_left(
  Graph,
  Text,
  [element(h3, [], [element(a, _, [element(h3, [], _)])]) | T]
):-
  !,
  dbnl_text_left(Graph, Text, T).
% Parse the content of paragraphs.
dbnl_text_left(Graph, Text, [element(p, [], H) | T]):-
  !,
  dbnl_text_left(Graph, Text, H),
  dbnl_text_left(Graph, Text, T).
% Justification text.
dbnl_text_left(
  Graph,
  Text,
  [
    element(
      p,
      [class=verantwoording],
      [element(a, Attributes, [verantwoording])]
    )
  | T]
):-
  !,
  memberchk(href=RelativeURI, Attributes),
  rdf(Text, dbnl:original_page, URI, Graph),
  uri_resolve(RelativeURI, URI, AbsoluteURI),
  dbnl_text(Graph, Text, AbsoluteURI, Justification),
  rdf_assert(Text, dbnl:justification, Justification, Graph),
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
% Author.
dbnl_text_left(
  Graph,
  Text,
  [AuthorAtom, element(a, Attributes, [AuthorName]) | T]
):-
  atom(AuthorAtom),
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
  atom(IllustratorAtom),
  strip([' ',':'], IllustratorAtom, 'illustrator'),
  !,
  memberchk(href=RelativeURI, Attributes),
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  dbnl_assert_author(Graph, AbsoluteURI, IllustratorName, Illustrator),
  rdf_assert(Text, dbnl:illustrator, Illustrator, Graph),
  dbnl_text_left(Graph, Text, T).
% Source.
dbnl_text_left(
  Graph,
  Text,
  [AuthorName1, element(i, [], [PublicationName]), H1 | T]
):-
  atom(H1),
  atom_concat('bron:', AuthorName2, AuthorName1),
  !,
  strip([' ',','], AuthorName2, AuthorName3),

  % Just checking... author name.
  (
    % No author name always works.
    AuthorName3 = ''
  ->
    true
  ;
    rdf(Text, dbnl:author, Author, Graph),
    rdfs_label(Author, AuthorName3),
    rdfs_label(Text, PublicationName)
  ->
    true
  ;
    gtrace %DEB
  ),
  strip([' ','.'], H1, H2),
  dbnl_text_left(Graph, Text, [H2 | T]).
% Journal year.
dbnl_text_left(Graph, Text, [H1 | T]):-
  atom(H1),
  atom_concat('Jaargang ', H3, H2),
  !,
  (
    split_atom_exclusive('.', H3, [JYear1, Rest]),
    atom_number(JYear1, JYear2),
    rdf_assert_datatype(Text, dbnl:journal_year, int, JYear2, Graph)
  ;
    Rest = H2
  ),
  dbnl_text_left(Graph, Text, [Rest | T]).
% Illustrations source.
dbnl_text_left(Graph, Text, [H1 | T]):-
  atom(H1),
  strip([' ','.'], H1, H2),
  atom_concat('Met illustraties van ', H3, H2),
  !,
  rdf_assert_literal(Text, dbnl:illustrations_source, H3, Graph),
  dbnl_text_left(Graph, Text, T).
% Publisher.
dbnl_text_left(Graph, Text, [H1 | T]):-
  atom(H1),
  split_atom_exclusive(',', PublisherPlaceYear, [Publisher1, PlaceYear]),
  !,
  strip([' '], Publisher1, Publisher2),
  rdf_assert_literal(Text, dbnl:publisher, Publisher2, Graph),
  
  % Publication place with or without a year at the end.
  (
    sub_atom(PlaceYear, _Before, 4, 0, Year1),
    atom_number(Year1, Year2)
  ->
    rdf_assert_datatype(Text, dbnl:year, int, Year2, Graph),
    sub_atom(PlaceYear, 0, _Length, 4, Place1)
  ;
    Place1 = PlaceYear
  ),
  strip([' '], Place1, Place2),
  rdf_assert_literal(Text, dbnl:publisher_place, Place2, Graph),

  dbnl_text_left(Graph, Text, T).
% Copyright.
dbnl_text_left(Graph, Text, [H | Contents]):-
  atom(H),
  dbnl_copyright(Graph, Text, H),
  !,
  dbnl_text_left(Graph, Text, Contents).
% Debug.
dbnl_text_left(Graph, Text, [H | T]):-
  gtrace, %DEB
  format(user_output, '~w\n', [H]),
  dbnl_text_left(Graph, Text, T).

