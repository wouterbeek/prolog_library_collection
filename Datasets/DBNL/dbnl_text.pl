:- module(
  dbnl_text,
  [
    dbnl_text/4, % +Graph:atom
                 % +Title:uri
                 % +URI:uri
                 % -Text:uri
    dbnl_text_content/3 % +Graph:atom
                        % +Text:uri
                        % +XML:dom
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
:- use_module(dbnl(dbnl_text_left)).
:- use_module(dbnl(dbnl_toc)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(rdf(rdf_build)).
:- use_module(standards(xpath_ext)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



dbnl_maximum_xmlliteral_length(1000).

dbnl_text(Graph, _Title, URI, Text):-
  rdf(Text, dbnl:original_page, URI, Graph),
  !.
dbnl_text(Graph, Title, URI, Text):-
  dbnl_uri_to_html(URI, DOM),
gtrace,
  dbnl_dom_center(DOM, Contents),
  % Disambiguate between different kinds of texts.
  (
    % Downloads
    xpath2(Contents, //h3(content), ['Downloads'])
  ->
    dbnl_assert_text(Graph, URI, Text),
    rdf_assert(Title, dbnl:downloads, Text, Graph),
    dbnl_downloads(Graph, Text, Contents)
  ;
    % Only checking, not retrieving.
    xpath2(Contents, //h2(@class=inhoud), _)
  ->
    % Table of contents.
    dbnl_assert_toc(Graph, URI, Text),
    rdf_assert(Title, dbnl:toc, Text, Graph),
    dbnl_text(Graph, Text, Contents)
  ;
    % Only checking, not retrieving.
    xpath2(Contents, //dt, DT),
    xpath2(DT, a, _)
  ->
    % Collection of volumes.
    dbnl_assert_volume_collection(Graph, URI, Text),
    rdf_assert(Title, dbnl:volume_collection, Text, Graph),
    dbnl_text(Graph, Text, Contents)
  ;
    % Only checking, not retrieving.
    xpath2(Contents, //div(@class=contentholder), _)
  ->
    % Actual content; DBNL markup.
    dbnl_assert_text(Graph, URI, Text),
    dbnl_dom_notes(DOM, Notes),
    dbnl_markup(
      [base_uri(URI), graph(Graph), notes(Notes), text(Text)],
      Contents,
      XML_DOM
    ),
    dbnl_text_content(Graph, Text, XML_DOM)
  ;
    % Any other text.
    dbnl_assert_text(Graph, URI, Text),
    dbnl_text(Graph, Text, Contents)
  ),
  
  rdf_assert(Text, dbnl:original_page, URI, Graph),
  
  % Parse left DIV.
  dbnl_dom_left(DOM, Left),
  dbnl_text_left(Graph, Text, Left),
  
  % Parse right DIV.
  dbnl_dom_left(DOM, Right),
  dbnl_text_right(Graph, Text, Right),
  !.
/*
  % Retrieve the colofon DOM.
  atom_concat(URI2, '/colofon.php', ColofonURI),
  dbnl_colofon(Graph, Title, ColofonURI),
  % Retrieve the downloads DOM.
  atom_concat(URI2, '/downloads.php', DownloadsURI),
  dbnl_downloads(Graph, Title, DownloadsURI),
  % Retrieve the index DOM.
  atom_concat(URI2, '/index.php', IndexURI),
  dbnl_process_text_index(Graph, Title, IndexURI),
dbnl_process_text_index(_Graph, _Title, URI):-
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents),
  write(Contents).
*/
% Debug.
dbnl_text(_Graph, _Title, URI, _Text):-
  gtrace, %DEB
  write(URI).

% Done!
dbnl_text(_Graph, _Text, []):-
  !.
% Skip empty headers.
dbnl_text(Graph, Text, [element(h2, _, []) | T]):-
  !,
  dbnl_text(Graph, Text, T).
% Nesting inside DIVs.
dbnl_text(Graph, Text, [element(div, _, Content) | Contents]):-
  !,
  dbnl_text(Graph, Text, Content),
  dbnl_text(Graph, Text, Contents).
% Table of contents.
dbnl_text(Graph, TOC, [element(h2, [class=inhoud], _) | Contents]):-
  !,
  dbnl_toc(Graph, TOC, Contents).
% Title.
% Note that the same title may already have been asserted
% for the TITLE resource, but not yet for this TEXT resource.
dbnl_text(
  Graph,
  Text,
  [element(h1, [class=title], Title1) | Contents]
):-
  !,
  dbnl_markup_simple(Title1, Title2),
  rdfs_assert_label(Text, Title2, Graph),
  dbnl_text(Graph, Text, Contents).
% Assert the supposed author.
% This is used in the left DIV to distinguish authors from editors.
dbnl_text(
  Graph,
  Text,
  [element(_, [class=author], [AuthorAtom]) | Contents]
):-
  !,
  dbnl_extract_author(AuthorAtom, AuthorName),
  rdf_assert(Text, dbnl:supposed_author, AuthorName, Graph),
  dbnl_text(Graph, Text, Contents).
% Assert the supposed editor.
% This is used in the left DIV to distinguish authors from editors.
dbnl_text(
  Graph,
  Text,
  [element(p, [class=editor], [EditorAtom]) | Contents]
):-
  !,
  gtrace,
  dbnl_extract_editor(EditorAtom, EditorName),
  rdf_assert_literal(Text, dbnl:supposed_editor, EditorName, Graph),
  dbnl_text(Graph, Text, Contents).
% Skip interp (what is this anyway?).
dbnl_text(Graph, Text, [element(interp, _, _) | Contents]):-
  !,
  dbnl_text(Graph, Text, Contents).
% Skip empty subordinate titles.
dbnl_text(
  Graph,
  Text,
  [element(h3, [class='title-subordinate'], []) | Contents]
):-
  !,
  dbnl_text(Graph, Text, Contents).
% Non-empty subordinate title.
dbnl_text(
  Graph,
  Text,
  [element(h3, [class='title-subordinate'], [Subtitle]) | Contents]
):-
  atom(Subtitle),
  !,
  rdf_assert_literal(Text, dbnl:subtitle, Subtitle, Graph),
  dbnl_text(Graph, Text, Contents).
% Skip empty paragraphs.
dbnl_text(Graph, Text, [element(p, _, []) | Contents]):-
  !,
  dbnl_text(Graph, Text, Contents).
% Skip notes on scans.
dbnl_text(
  Graph,
  Text,
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
  dbnl_text(Graph, Text, Contents).
% Skip linebreaks.
dbnl_text(Graph, Text, [element(br, _, _) | Contents]):-
  !,
  dbnl_text(Graph, Text, Contents).
% Parse the contents of paragraphs.
dbnl_text(Graph, Text, [element(p, [], Content) | Contents]):-
  !,
  dbnl_text(Graph, Text, Content),
  dbnl_text(Graph, Text, Contents).
% Parse definition lists.
dbnl_text(Graph, Text, [element(dl, [], DTs) | Contents]):-
  !,
  dbnl_text_definition_list(Graph, Text, DTs),
  dbnl_text(Graph, Text, Contents).
% Skip links to the PDF files, since we will take these very same files
% from the 'downloads' page.
dbnl_text(
  Graph,
  Text,
  [element(a, _Attributes, ['Bekijk het PDF bestand.']) | Contents]
):-
  !,
  dbnl_text(Graph, Text, Contents).
% Skip PDF file media link.
dbnl_text(Graph, Text, [element(a, Attributes, ['PDF File']) | Contents]):-
  memberchk(class=media, Attributes),
  !,
  dbnl_text(Graph, Text, Contents).
% Skip single space atomic content.
dbnl_text(Graph, Text, ['\240\' | Contents]):-
  !,
  dbnl_text(Graph, Text, Contents).
% Copyright atom.
dbnl_text(Graph, Text, [Atom | Contents]):-
  dbnl_copyright(Graph, Text, Atom),
  !,
  dbnl_text(Graph, Text, Contents).
% Debug.
dbnl_text(Graph, Text, [Content | Contents]):-
  gtrace, %DEB
  format(user_output, '~w\n', [Content]),
  dbnl_text(Graph, Text, Contents).

%! dbnl_text_content(+Graph:atom, +Text:uri, +XML:dom) is det.
% Associates XML content with a text resource.

dbnl_text_content(Graph, Text, XML_DOM):-
  dom_to_xml(dbnl, XML_DOM, XML),
  rdf(Text, dbnl:original_page, URI, Graph),
  atom_length(XML, Length),
  dbnl_maximum_xmlliteral_length(MaxLength),
  (
    Length > MaxLength
  ->
    uri_components(URI, Components),
    uri_data(path, Components, Path),
    file_name(Path, _Directory, Name, _Extension),
    absolute_file_name(file(Name), File, [file_type(xml)]),
    xml_current_namespace(xlink, XLinkNamespace),
    dom_to_xml_file(dbnl, XML_DOM, File, [nsmap([xlink=XLinkNamespace])]),
    rdf_assert_datatype(Text, dbnl:long_content, file, File, Graph)
  ;
    rdf_assert_xml_literal(Text, dbnl:short_content, XML, Graph)
  ).

% Skip DBNL logo.
dbnl_text_definition_list(_Graph, _Text, [element(dt, [], [Element])]):-
  dbnl_logo(Element),
  !.
dbnl_text_definition_list(Graph, Text, DTs):-
  maplist(dbnl_text_definition_term(Graph, Text), DTs).

% Skip empty definition term.
dbnl_text_definition_term(_Graph, _Text, element(dt, [], [])):-
  !.
% A work that consits of multiple parts.
dbnl_text_definition_term(
  Graph,
  VolumeCollection,
  element(dt, [], [element(a, Attributes, TitleName1), _BR1, _BR2])
):-
  memberchk(href=RelativeURI, Attributes),
  !,
  dbnl_uri_resolve(RelativeURI, AbsoluteURI),
  dbnl_text(Graph, VolumeCollection, AbsoluteURI, Subtext),
  rdf_assert(VolumeCollection, dbnl:volume, Subtext, Graph),
  dbnl_markup(
    [base_uri(AbsoluteURI), graph(Graph), text(VolumeCollection)],
    TitleName1,
    TitleName2
  ),
  rdf_assert_xml_literal(Subtext, dbnl:title, TitleName2, Graph).

% Done!
dbnl_text_right(_Graph, _Text, []):-
  !.
% Image of the titlepage.
dbnl_text_right(Graph, Text, [element(a, _, [element(img, IMGs, [])]) | T]):-
  memberchk(alt=titelpagina, IMGs),
  !,
  memberchk(src=RelativeURI, IMGs),
  rdf(Text, dbnl:original_page, BaseURI, Graph),
  dbnl_uri_resolve(RelativeURI, BaseURI, AbsoluteURI),
  
  absolute_file_name(file(RelativeURI), File, []),
  uri_to_file(AbsoluteURI, File),
  rdf_assert_datatype(Text, dbnl:titlepage_image, image, File, Graph),
  
  dbnl_text_right(Graph, Text, T).

