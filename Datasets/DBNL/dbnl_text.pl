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
@tbd Parse colofon pages that are linked to from within the left DIV.
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_downloads)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_markup)).
:- use_module(dbnl(dbnl_text_left)).
:- use_module(dbnl(dbnl_toc)).
:- use_module(dcg(dcg_generic)).
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
  dbnl_dom_center(DOM, Contents),
  % Disambiguate between different kinds of texts.
  (
    % Downloads
    xpath2(Contents, //h3(content), ['Downloads'])
  ->
    dbnl_assert_text(Graph, URI, Text),
    rdf_assert(Title, dbnl:downloads, Text, Graph),
gtrace,
    phrase(dbnl_downloads(Graph, Text), Contents, [])
  ;
    % Scans, also for download.
    xpath2(Contents, //div(@class='pb-scan'), _)
  ->
    dbnl_assert_text(Graph, URI, Text),
    rdf_assert(Title, dbnl:downloads, Text, Graph),
gtrace,
    dbnl_scans(Graph, Text, Contents)
  ;
    % Only checking, not retrieving.
    xpath2(Contents, //h2(@class=inhoud), _)
  ->
    % Table of contents.
    dbnl_assert_toc(Graph, URI, Text),
    rdf_assert(Title, dbnl:toc, Text, Graph),
gtrace,
    phrase(dbnl_text0(Graph, Text), Contents, [])
  ;
    % Only checking, not retrieving.
    xpath2(Contents, //dt, DT),
    xpath2(DT, a, _)
  ->
    % Collection of volumes.
    dbnl_assert_volume_collection(Graph, URI, Text),
    rdf_assert(Title, dbnl:volume_collection, Text, Graph),
gtrace,
    phrase(dbnl_text0(Graph, Text), Contents, [])
  ;
    % Only checking, not retrieving.
    xpath2(Contents, //div(@class=contentholder), _)
  ->
    % Actual content; DBNL markup.
    dbnl_assert_text(Graph, URI, Text),
    dbnl_dom_notes(DOM, Notes),
gtrace,
    dbnl_markup(
      [base_uri(URI), graph(Graph), notes(Notes), text(Text)],
      Contents,
      XML_DOM
    ),
    dbnl_text_content(Graph, Text, XML_DOM)
  ;
    % Any other text.
    dbnl_assert_text(Graph, URI, Text),
gtrace,
    phrase(dbnl_text0(Graph, Text), Contents, [])
  ),

  rdf_assert(Text, dbnl:original_page, URI, Graph),

  % Parse left DIV.
  dbnl_dom_left(DOM, Left),
  phrase(dbnl_text_left(Graph, Text), Left, []),

  % Parse right DIV.
  dbnl_dom_right(DOM, Right),
  phrase(dbnl_text_right(Graph, Text), Right),
  !.
% Debug.
dbnl_text(_Graph, _Title, URI, _Text):-
  gtrace, %DEB
  write(URI).

dbnl_text0(_Graph, _Text) --> [].
% Skip empty headers.
dbnl_text0(Graph, Text) -->
  [element(h2, _, [])],
  !,
  dbnl_text0(Graph, Text).
% Nesting inside DIVs.
dbnl_text0(Graph, Text) -->
  [element(div, _, Content)],
  !,
  {phrase(dbnl_text0(Graph, Text), Content)},
  dbnl_text0(Graph, Text).
% Table of contents.
dbnl_text0(Graph, TOC, [element(h2, [class=inhoud], [TOC_Title]) | Contents], []):-
  !,
  dcg_phrase(dbnl_toc_title(Type, Name), TOC_Title),
  write(Type),
  write(Name),
  dbnl_toc(Graph, TOC, Contents).
% Title.
% Note that the same title may already have been asserted
% for the TITLE resource, but not yet for this TEXT resource.
dbnl_text0(Graph, Text) -->
  [element(h1, [class=title], Title1)],
  !,
  {
    dbnl_markup_simple(Title1, Title2),
    rdfs_assert_label(Text, Title2, Graph)
  },
  dbnl_text0(Graph, Text).
% Assert the supposed author.
% This is used in the left DIV to distinguish authors from editors.
dbnl_text0(Graph, Text) -->
  [element(_, [class=author], [Author])],
  !,
  {
    dcg_phrase(dbnl_author(AuthorName), Author),
    rdf_assert(Text, dbnl:supposed_author, AuthorName, Graph)
  },
  dbnl_text0(Graph, Text).
% Assert the supposed editor.
% This is used in the left DIV to distinguish authors from editors.
dbnl_text0(Graph, Text) -->
  [element(p, [class=editor], [Editor])],
  !,
  {
    dcg_phrase(dbnl_editor(EditorName), Editor),
    rdf_assert_literal(Text, dbnl:supposed_editor, EditorName, Graph)
  },
  dbnl_text0(Graph, Text).
% Skip interp (what is this anyway?).
dbnl_text0(Graph, Text) -->
  [element(interp, _, _)],
  !,
  dbnl_text0(Graph, Text).
% Skip empty subordinate titles.
dbnl_text0(Graph, Text) -->
  [element(h3, [class='title-subordinate'], [])],
  !,
  dbnl_text0(Graph, Text).
% Non-empty subordinate title.
dbnl_text0(Graph, Text) -->
  [element(h3, [class='title-subordinate'], [Subtitle])],
  {atom(Subtitle)},
  !,
  {rdf_assert_literal(Text, dbnl:subtitle, Subtitle, Graph)},
  dbnl_text0(Graph, Text).
% Skip empty paragraphs.
dbnl_text0(Graph, Text) -->
  [element(p, _, [])],
  !,
  dbnl_text0(Graph, Text).
% Skip notes on scans.
dbnl_text0(Graph, Text) -->
  [element(h4, [], [Atom])],
  {memberchk(
    Atom,
    [
      '* Van dit werk zijn alleen scans beschikbaar. Voor een snelle oriëntatie is hieronder een viewer beschikbaar.',
      '* Bij deze tekst wordt ook de mogelijkheid geboden om de originele pagina\'s te bekijken via de knop ‘origineel’ naast het paginanummer.'
    ]
  )},
  !,
  dbnl_text0(Graph, Text).
% Skip linebreaks.
dbnl_text0(Graph, Text) -->
  [element(br, _, _)],
  !,
  dbnl_text0(Graph, Text).
% Parse the contents of paragraphs.
dbnl_text0(Graph, Text) -->
  [element(p, [], C)],
  !,
  {phrase(dbnl_text0(Graph, Text), C)},
  dbnl_text0(Graph, Text).
% Parse definition lists.
dbnl_text0(Graph, Text) -->
  [element(dl, [], DTs)],
  !,
  {phrase(dbnl_text_definition_list(Graph, Text), DTs)},
  dbnl_text0(Graph, Text).
% Skip links to the PDF files, since we will take these very same files
% from the 'downloads' page.
dbnl_text0(Graph, Text) -->
  [element(a, _Attributes, ['Bekijk het PDF bestand.'])],
  !,
  dbnl_text0(Graph, Text).
% Skip PDF file media link.
dbnl_text0(Graph, Text) -->
  [element(a, Attrs, ['PDF File'])],
  !,
  {memberchk(class=media, Attrs)},
  dbnl_text0(Graph, Text).
% Skip single space atomic content.
dbnl_text0(Graph, Text) -->
  ['\240\'],
  !,
  dbnl_text0(Graph, Text).
% Copyright atom.
dbnl_text0(Graph, Text) -->
  [Atom],
  {dcg_phrase(dbnl_copyright(Graph, Text), Atom)},
  !,
  dbnl_text0(Graph, Text).
% Debug.
dbnl_text0(_Graph, _Text) -->
  dcg_debug.

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
dbnl_text_definition_list(_Graph, _Text) --> [].
dbnl_text_definition_list(_Graph, _Text) -->
  [element(dt, [], [Element])],
  !,
  {phrase(dbnl_logo, Element)}.
dbnl_text_definition_list(Graph, Text) -->
  dbnl_text_definition_term(Graph, Text),
  dbnl_text_definition_list(Graph, Text).

% Skip empty definition term.
dbnl_text_definition_term(_Graph, _Text) -->
  [element(dt, [], [])],
  !.
% A work that consits of multiple parts.
dbnl_text_definition_term(Graph, VolumeCollection) -->
  [element(dt, [], [element(a, Attrs, TitleName1), _BR1, _BR2])],
  !,
  {
    memberchk(href=RelativeURI, Attrs),
    dbnl_uri_resolve(RelativeURI, AbsoluteURI),
    dbnl_text(Graph, VolumeCollection, AbsoluteURI, Subtext),
    rdf_assert(VolumeCollection, dbnl:volume, Subtext, Graph),
    dbnl_markup(
      [base_uri(AbsoluteURI), graph(Graph), text(VolumeCollection)],
      TitleName1,
      TitleName2
    ),
    rdf_assert_xml_literal(Subtext, dbnl:title, TitleName2, Graph)
  }.

% Image of the titlepage.
dbnl_text_right(Graph, Text) -->
  [element(a, _, [element(img, Attrs, [])])],
  !,
  {
    memberchk(alt=titelpagina, Attrs),
    memberchk(src=RelativeURI, Attrs),
    rdf(Text, dbnl:original_page, BaseURI, Graph),
    dbnl_uri_resolve(RelativeURI, BaseURI, AbsoluteURI),
    absolute_file_name(file(RelativeURI), File, []),
    uri_to_file(AbsoluteURI, File),
    rdf_assert_datatype(Text, dbnl:titlepage_image, image, File, Graph)
  },
  dbnl_text_right(Graph, Text).
% Done!
dbnl_text_right(_Graph, _Text) --> [].

toc_title(journal, Title) -->
  "Jaargangen van het tijdschrift",
  blank,
  string(Title).

