:- module(
  dbnl_text,
  [
    dbnl_text/3 % +Graph:atom
                % +Title:uri
                % +URI:uri
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
:- use_module(dbnl(dbnl_toc)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(library(uri)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_text(+Graph:atom, +Title:uri, +URI:uri) is det.
% Process the text that is located at the given URI.
%
% There are three types of pages that may be accessible from the text page:
%   1. Colofon page.
%   2. Downloads page.
%   3. Index page.
%
% @tbd Could uri_resolve/3 be used instead of atom_concat/3?

dbnl_text(Graph, Title, URI1):-
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
  dbnl_text(Graph, Title, URI2, Contents),

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
dbnl_text(_Graph, _Title, URI):-
  gtrace, %DEB
  write(URI).

%! dbnl_text(+Graph:atom, +Title:uri, +URI:uri, +HTML:dom) is det.

% Done!
dbnl_text(_Graph, _Title, _URI, []):-
  !.
% Editor.
dbnl_text(
  Graph,
  Title,
  URI,
  [element(p, [class=editor], [EditorAtom]) | Contents]
):-
  !,
  dbnl_extract_editor(EditorAtom, EditorName),
  dbnl_assert_editor(Graph, EditorName, Editor),
  rdf_assert(Title, dbnl:editor, Editor, Graph),
  dbnl_text(Graph, Title, URI, Contents).
% Title.
dbnl_text(
  Graph,
  Title,
  URI,
  [element(h1, [class=title], [TitleName]) | Contents]
):-
  !,
  % Note that some publications have multiple titles.
  % For example: =|http://www.dbnl.org/titels/titel.php?id=lint011gesc00|=.
  rdfs_assert_label(Title, TitleName, Graph),
  dbnl_text(Graph, Title, URI, Contents).
% Table of contents.
dbnl_text(
  Graph,
  Title,
  URI,
  [element(h2, [class=inhoud], _) | Contents]
):-
  !,
  dbnl_toc(Graph, Title, URI, Contents).
% Author.
dbnl_text(
  Graph,
  Title,
  URI,
  [element(_, [class=author], [AuthorName]) | Contents]
):-
  !,
  % Just checking...
  rdf(Title, dbnl:author, Author, Graph),
  (rdfs_label(Author, AuthorName) -> true ; gtrace),
  dbnl_text(Graph, Title, URI, Contents).
% Skip empty author.
dbnl_text(
  Graph,
  Title,
  URI,
  [element(_, [class=author], []) | Contents]
):-
  !,
  dbnl_text(Graph, Title, URI, Contents).
% Skip empty subordinate titles.
dbnl_text(
  Graph,
  Title,
  URI,
  [element(h3, [class='title-subordinate'], []) | Contents]
):-
  !,
  dbnl_text(Graph, Title, URI, Contents).
% Skip empty paragraphs.
dbnl_text(Graph, Title, URI, [element(p, _, []) | Contents]):-
  !,
  dbnl_text(Graph, Title, URI, Contents).
% Skip notes on scans.
dbnl_text(
  Graph,
  Title,
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
  dbnl_text(Graph, Title, URI, Contents).
% Skip linebreaks.
dbnl_text(Graph, Title, URI, [element(br, _, _) | Contents]):-
  !,
  dbnl_text(Graph, Title, URI, Contents).
% Copyright atom.
dbnl_text(Graph, Title, URI, [Atom | Contents]):-
gtrace,
  dbnl_extract_copyright(Atom, Organization, Year),
  !,
  dbnl_assert_copyright(Graph, Organization, Year, Copyright),
  rdf_assert(Title, dbnl:copyright, Copyright, Graph),
  dbnl_text(Graph, Title, URI, Contents).
% Debug.
dbnl_text(Graph, Title, URI, [Content | Contents]):-
  gtrace, %DEB
  format(user_output, '~w\n', [Content]),
  dbnl_text(Graph, Title, URI, Contents).

% @tbd What is this?
dbnl_process_text_index(_Graph, _Title, URI):-
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents),
  write(Contents).

