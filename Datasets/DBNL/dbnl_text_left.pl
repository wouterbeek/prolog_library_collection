:- module(
  dbnl_text_left,
  [
    dbnl_text_left//2 % +Graph:atom
                      % +Text:uri
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
:- use_module(dcg(dcg_generic)).
:- use_module(generics(atom_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_text_left(+Graph:atom, +Text:uri)// is det.
% @tbd Enable justification text parsing.

% Skip title.
dbnl_text_left(Graph, Text) -->
  [element(h3, [], [element(a, _, [element(h3, [], _)])])],
  !,
  dbnl_text_left(Graph, Text).
% Parse the content of paragraphs.
dbnl_text_left(Graph, Text) -->
  [element(p, [], H)],
  !,
  {phrase(dbnl_text_left(Graph, Text), H)},
  dbnl_text_left(Graph, Text).
% Justification text.
dbnl_text_left(Graph, Text) -->
  [
    element(
      p,
      [class=verantwoording],
      [element(a, Attrs, [verantwoording])]
    )
  ],
  {(
    member(class=chosen, Attrs)
  ->
    true
  ;
    memberchk(href=_RelativeURI, Attrs)
    %rdf(Text, dbnl:original_page, URI, Graph),
    %dbnl_uri_resolve(RelativeURI, URI, AbsoluteURI),
    %dbnl_text(Graph, Text, AbsoluteURI, Justification),
    %rdf_assert(Text, dbnl:justification, Justification, Graph)
  )},
  !,
  dbnl_text_left(Graph, Text).
% Table of contents text.
dbnl_text_left(Graph, Text) -->
  [
    element(
      p,
      [class=inhoudsopgave],
      [element(a, Attrs, [inhoudsopgave])]
    )
  ],
  {(
    member(class=chosen, Attrs)
  ->
    true
  ;
    memberchk(href=RelativeURI, Attrs),
    rdf(Text, dbnl:original_page, URI, Graph),
    dbnl_uri_resolve(RelativeURI, URI, AbsoluteURI),
    dbnl_text(Graph, Text, AbsoluteURI, TOC),
    rdf_assert(Text, dbnl:toc, TOC, Graph)
  )},
  !,
  dbnl_text_left(Graph, Text).
% Downloads text.
dbnl_text_left(Graph, Text) -->
  [element(a, Attrs, [downloads])],
  {(
    member(class=chosen, Attrs)
  ->
    true
  ;
    memberchk(href=RelativeURI, Attrs),
    rdf(Text, dbnl:original_page, URI, Graph),
    dbnl_uri_resolve(RelativeURI, URI, AbsoluteURI),
    dbnl_text(Graph, Text, AbsoluteURI, Downloads),
    rdf_assert(Text, dbnl:downloads, Downloads, Graph)
  )},
  !,
  dbnl_text_left(Graph, Text).
% Skip search.
dbnl_text_left(Graph, Text) -->
  [element(a, _Attributes, ['doorzoek de hele tekst'])],
  !,
  dbnl_text_left(Graph, Text).
% Parse the content of DIVs.
dbnl_text_left(Graph, Text) -->
  [element(div, [], H)],
  !,
  {phrase(dbnl_text_left(Graph, Text), H)},
  dbnl_text_left(Graph, Text).
% Skip line breaks.
dbnl_text_left(Graph, Text) -->
  [element(br, _, [])],
  !,
  dbnl_text_left(Graph, Text).
% Author / editor.
dbnl_text_left(Graph, Text) -->
  [Prefix, element(a, Attrs, [AuthorName])],
  {
    memberchk(Prefix, ['auteur:','editeur:']),
    memberchk(href=RelativeURI, Attrs),
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
    )
  },
  dbnl_text_left(Graph, Text).
% Empty author.
dbnl_text_left(Graph, Text) -->
  ['auteur:'],
  !,
  dbnl_text_left(Graph, Text).
% Illustrator.
dbnl_text_left(Graph, Text) -->
  [IllustratorAtom, element(a, Attrs, [IllustratorName])],
  {
    atom(IllustratorAtom),
    strip_atom([' ',':'], IllustratorAtom, 'illustrator'),
    !,
    memberchk(href=RelativeURI, Attrs),
    dbnl_uri_resolve(RelativeURI, AbsoluteURI),
    dbnl_assert_author(Graph, AbsoluteURI, IllustratorName, Illustrator),
    rdf_assert(Text, dbnl:illustrator, Illustrator, Graph)
  },
  dbnl_text_left(Graph, Text).
% Source.
dbnl_text_left(Graph, Text) -->
  ['bron:', element(i, [], [JournalBook]), YearPublisherCity],
  {
    atom(JournalBook),
    atom(YearPublisherCity),
    !,
    % TOO DIFFICULT TO FULLY PROCESS!
    %dcg_phrase(dbnl_journal(Graph, Text), Journal),
    %dcg_phrase(dbnl_year(Graph, Text), Year),
    rdf_assert_literal(Text, dbnl:journal_book, JournalBook, Graph),
    rdf_assert_literal(Text, dbnl:year_publisher_city, YearPublisherCity, Graph)
  },
  dbnl_text_left(Graph, Text).
% Publication source (in book, in journal, publisher, city ,year).
dbnl_text_left(Graph, Text) -->
  [Atom],
  {
    atom(Atom),
    atom_concat('bron: ', Source, Atom),
    rdf_assert_literal(Text, dbnl:source, Source, Graph)
  },
  dbnl_text_left(Graph, Text).
% Illustrations source.
dbnl_text_left(Graph, Text) -->
  [H1],
  {
    atom(H1),
    strip_atom([' ','.'], H1, H2),
    atom_concat('Met illustraties van ', H3, H2),
    !,
    rdf_assert_literal(Text, dbnl:illustrations_source, H3, Graph)
  },
  dbnl_text_left(Graph, Text).
% Publisher.
dbnl_text_left(Graph, Text) -->
  [H1],
  {
    atom(H1),
    split_atom_exclusive(',', H1, [Publisher1, PlaceYear]),
    !,
    strip_atom([' '], Publisher1, Publisher2),
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
    strip_atom([' '], Place1, Place2),
    rdf_assert_literal(Text, dbnl:publisher_place, Place2, Graph)
  },
  dbnl_text_left(Graph, Text).
% Copyright.
dbnl_text_left(Graph, Text) -->
  dbnl_copyright(Graph, Text),
  !,
  dbnl_text_left(Graph, Text).
% Logo of DBNL.
dbnl_text_left(Graph, Text) -->
  dbnl_logo,
  !,
  dbnl_text_left(Graph, Text).
% Logo of Rotterdam library.
dbnl_text_left(Graph, Text) -->
  [_, element(a, _, [element(img, Attrs, [])])],
  !,
  {
    memberchk(id='funder-logo', Attrs),
    memberchk(src=RelativeURI, Attrs),
    dbnl_uri_resolve(RelativeURI, AbsoluteURI),
    dbnl_save_image(Graph, AbsoluteURI, BNode),
    rdf_assert(Text, dbnl:funder, BNode, Graph)
  },
  dbnl_text_left(Graph, Text).
% Done!
dbnl_text_left(_Graph, _Text) --> [], !.
% Debug.
dbnl_text_left(_Graph, _Text) -->
  dcg_debug.

