:- module(
  dbnl_text,
  [
    dbnl_text/4 % +Graph:atom
                % +Title:uri
                % +URI:uri
                % -Text:uri
  ]
).

/** <module> DBNL MARKUP

Process DBNL text markup.

---+ Content

---++ Left

  * author:uri
  * dbnl_logo:file
  * doorzoek_de_hele_teskt:uri
  * downloads:uri
  * editors:list(uri)
  * inhoudsopgave:uri
  * rights:atom
  * source:atom
    * author:atom
    * city:atom
    * editors:list(atom)
    * publisher:atom
    * title:atom
    * volume:atom
    * year:atom
  * title:atom
  * verantwoording:uri

---++ Center

  * bibliography:list(atom)
    BIBLIOGRAPHY
  * author:atom
  * contents:list(uri)
    TABLE_OF_CONTENTS
  * dbnl_markup:dom
    PAGE
  * title:atom

---++ Right

  * image:file
  * notes:list(atom,dom)

---+ URI

==
http://www.dbnl.org/tekst/ferr002atma01_01/
http://www.dbnl.org/tekst/ferr002atma01_01/ferr002atma01_01_0006.php
==

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_downloads)).
:- use_module(dbnl(dbnl_extract)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_markup)).
:- use_module(dbnl(dbnl_toc)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').


dbnl_text(Graph, _Title, URI, Text):-
  rdf(Text, dbnl:original_page, URI, Graph),
  !.
dbnl_text(Graph, Title, URI1, Text):-
(URI1 = 'http://www.dbnl.org/tekst/_nee003190801_01' -> gtrace ; true),
  % There are several possibilities here:
  %   1. The URI already refers to a PHP script.
  %   2. ...
  uri_components(
    URI1,
    uri_components(_Scheme, _Authority, Path, _Search, _Fragment)
  ),
  (
    file_name_extension(_Base, php, Path)
  ->
    URI2 = URI1
  ;
    atom_concat(_, '_01', URI1)
  ->
    URI2 = URI1
  ;
    atomic_concat(URI1, '_01', URI2)
  ),

  dbnl_uri_to_html(URI2, DOM),
  dbnl_dom_center(DOM, Contents),

  % Sometimes the page itself is a text.
  dbnl_assert_text(Graph, URI2, Text),
  dbnl_text0(Graph, Text, URI2, Contents),

/*
  % Retrieve the colofon DOM.
  atom_concat(URI2, '/colofon.php', ColofonURI),
  dbnl_colofon(Graph, Title, ColofonURI),
*/
  % Retrieve the downloads DOM.
  atom_concat(URI2, '/downloads.php', DownloadsURI),
  dbnl_downloads(Graph, Title, DownloadsURI),
/*
  % Retrieve the index DOM.
  atom_concat(URI2, '/index.php', IndexURI),
  dbnl_process_text_index(Graph, Title, IndexURI),
*/
  !.
% Debug.
dbnl_text(_Graph, _Text, URI):-
  gtrace, %DEB
  write(URI).

% Done!
dbnl_text0(_Graph, _Text, _URI, []):-
  !.
% Editor.
dbnl_text0(
  Graph,
  Text,
  URI,
  [element(p, [class=editor], [EditorAtom]) | Contents]
):-
  !,
  dbnl_extract_editor(EditorAtom, EditorName),
  dbnl_assert_editor(Graph, EditorName, Editor),
  rdf_assert(Text, dbnl:editor, Editor, Graph),
  dbnl_text0(Graph, Text, URI, Contents).
% Title.
% Note that some publications have multiple titles.
% For example: =|http://www.dbnl.org/titels/titel.php?id=lint011gesc00|=.
% @tbd Is this really the case?!
dbnl_text0(
  Graph,
  Text,
  URI,
  [element(h1, [class=title], [TitleName]) | Contents]
):-
  !,
  % Just checking...
  (rdfs_label(Text, TitleName) -> true ; gtrace),
  dbnl_text0(Graph, Text, URI, Contents).
% Table of contents.
dbnl_text0(
  Graph,
  Text,
  URI,
  [element(h2, [class=inhoud], _) | Contents]
):-
  !,
  dbnl_toc(Graph, Text, URI, Contents).
% Author.
dbnl_text0(
  Graph,
  Text,
  URI,
  [element(_, [class=author], [AuthorName]) | Contents]
):-
  !,
  % Just checking...
  rdf(Text, dbnl:author, Author, Graph),
  (rdfs_label(Author, AuthorName) -> true ; gtrace),
  dbnl_text0(Graph, Text, URI, Contents).
% Skip empty author.
dbnl_text0(
  Graph,
  Text,
  URI,
  [element(_, [class=author], []) | Contents]
):-
  !,
  dbnl_text0(Graph, Text, URI, Contents).
% Skip empty subordinate titles.
dbnl_text0(
  Graph,
  Text,
  URI,
  [element(h3, [class='title-subordinate'], []) | Contents]
):-
  !,
  dbnl_text0(Graph, Text, URI, Contents).
% Skip empty paragraphs.
dbnl_text0(Graph, Text, URI, [element(p, _, []) | Contents]):-
  !,
  dbnl_text0(Graph, Text, URI, Contents).
% Skip notes on scans.
dbnl_text0(
  Graph,
  Text,
  URI,
  [
    element(
      h4,
      [],
      ['* Van dit werk zijn alleen scans beschikbaar. Voor een snelle oriëntatie is hieronder een viewer beschikbaar.']
    )
  | Contents
  ]
):-
  !,
  dbnl_text0(Graph, Text, URI, Contents).
% Skip linebreaks.
dbnl_text0(Graph, Text, URI, [element(br, _, _) | Contents]):-
  !,
  dbnl_text0(Graph, Text, URI, Contents).
% Parse the contents of paragraphs.
dbnl_text0(Graph, Text, URI, [element(p, [], Content) | Contents]):-
  !,
  dbnl_text0(Graph, Text, URI, Content),
  dbnl_text0(Graph, Text, URI, Contents).
dbnl_text0(Graph, Text, URI, [element(dl, [], DTs) | Contents]):-
  !,
  dbnl_text_definition_list(Graph, Text, URI, DTs),
  dbnl_text0(Graph, Text, URI, Contents).
% Copyright atom.
dbnl_text0(Graph, Text, URI, [Atom | Contents]):-
  atom(Atom),
  dbnl_extract_copyright(Atom, Organization, Year),
  !,
  dbnl_assert_copyright(Graph, Organization, Year, Copyright),
  rdf_assert(Text, dbnl:copyright, Copyright, Graph),
  dbnl_text0(Graph, Text, URI, Contents).
% Debug.
dbnl_text0(Graph, Text, URI, [Content | Contents]):-
  gtrace, %DEB
  format(user_output, '~w\n', [Content]),
  dbnl_text0(Graph, Text, URI, Contents).

% Skip DBNL logo.
dbnl_text_definition_list(
  _Graph,
  _Text,
  _URI,
  [element(dt, [], [element(img, Attributes, [])])]
):-
  memberchk(alt='DBNL vignet', Attributes),
  memberchk(src='../dbnllogi.gif', Attributes),
  !.

dbnl_text_definition_list(Graph, Text, URI, DTs):-
  maplist(dbnl_text_definition_term(Graph, Text, URI), DTs).

% Skip empty definition term.
dbnl_text_definition_term(_Graph, _Text, _URI, element(dt, [], [])):-
  !.
% A work that consits of multiple parts.
dbnl_text_definition_term(
  Graph,
  Text,
  URI,
  element(dt, [], [element(a, Attributes, Content1), _BR1, _BR2])
):-
  memberchk(href=RelativeURI, Attributes),
  !,
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  dbnl_assert_part(Graph, AbsoluteURI, Part),
  dbnl_markup([graph(Graph), title(Part), uri(URI)], Content1, Content2),
  dom_to_xml(dbnl, Content2, XML_Content),
  rdf_assert_xml_literal(Part, dbnl:title, XML_Content, Graph),
  rdf_assert(Text, dbnl:part, Part, Graph).

% @tbd What is this?
dbnl_process_text_index(_Graph, _Title, URI):-
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents),
  write(Contents).

