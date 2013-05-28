:- module(
  dbnl_markup,
  [
    dbnl_markup/3 % +Options:list(nvpair)
                  % +HTML:dom
                  % -XML:dom
  ]
).

/** <module> DBNL MARKUP

Predicates for transforming DBNL HTML markup into a useful XML format.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_extract)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(generics(atom_ext)).
:- use_module(generics(uri_ext)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xlink)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').
:- xml_register_namespace(xlink, 'http://www.w3.org/1999/xlink').



%! dbnl_indexed_lines(+Options:list(nvpair), +TRs:dom, -XML:dom) is det.

dbnl_indexed_lines(_Options, [], []):-
  flag(indexed_content, _OldID, 0),
  !.
dbnl_indexed_lines(
  Options,
  [TR | TRs],
  [element(iline, [index=Index2], Content2) | IndexedContent]
):-
  % The index part.
  xpath_chk(TR, td(1,content), [Index1]),
  (
    % A number rests the counter.
    atom_number(Index1, Index2)
  ->
    flag(indexed_content, _OldID, Index2)
  ;
    % A space uses the previous counter, if any.
    Index1 == '\240\'
  ->
    (
      flag(indexed_content, 0, 0)
    ->
      Index2 = 0
    ;
      flag(indexed_content, Index, Index + 1),
      Index2 is Index + 1
    )
  ;
    % A non-numberic, non-space index is left intact.
    Index2 = Index1
  ),

  % The content part.
  xpath_chk(TR, td(2,content), Content1),
  dbnl_markup(Options, Content1, Content2),

  dbnl_indexed_lines(Options, TRs, IndexedContent).

% Done!
dbnl_markup(_Options, [], []):-
  !.
% A single space.
dbnl_markup(_Options, ['\240\'], []):-
  !.
% A page. Nesting makes this relatively difficult.
dbnl_markup(
  Options,
  [element(div, Attributes, [PageAtom]) | Contents1],
  [element(page, [name=Page], []) | Contents2]
):-
  memberchk(class=pb, Attributes),
  !,
  dbnl_extract_page(PageAtom, Page),
  dbnl_markup(Options, Contents1, Contents2).
% Several dedicated DIV classes related to poetry.
dbnl_markup(
  Options,
  [element(div, Attributes, DIV_Contents1) | Contents1],
  Contents2
):-
  memberchk(class=Class, Attributes),
  (
    atom_concat('tabs-', _, Class)
  ;
    memberchk(
      Class,
      [
        line,
        'line-content',
        'line-nr',
        poem,
        'poem-small-margins'
      ]
    )
  ),
  !,
  dbnl_markup(Options, DIV_Contents1, DIV_Contents2),
  dbnl_markup(Options, Contents1, Contents3),
  append(DIV_Contents2, Contents3, Contents2).
% Disregard DIV tags.
dbnl_markup(
  Options,
  [element(div, Attributes, Contents1)],
  Contents2
):-
  memberchk(class=Class, Attributes),
  memberchk(Class, ['line-content-container']),
  !,
  (
    xpath(Contents1, //img, _)
  ->
    Contents2 = Contents3
  ;
    Contents2 = [element(line, [], Contents3)]
  ),
  dbnl_markup(Options, Contents1, Contents3).
% Header.
dbnl_markup(
  Options,
  [element(h3, _, H3_Contents) | Contents1],
  [element(header, [], SubheaderContents) | Contents2]
):-
  !,
  dbnl_markup(Options, H3_Contents, SubheaderContents),
  dbnl_markup(Options, Contents1, Contents2).
% Subheader.
dbnl_markup(
  Options,
  [element(h4, _, H4_Contents) | Contents1],
  [element(subheader, [], SubheaderContents) | Contents2]
):-
  !,
  dbnl_markup(Options, H4_Contents, SubheaderContents),
  dbnl_markup(Options, Contents1, Contents2).
% Image with caption.
dbnl_markup(
  Options,
  [
    element(
      p,
      _,
      [_BR1, _BR2, element(img, IMG_Attributes, []) | _]
    ),
    element(div, _, Caption1)
  | Contents1
  ],
  [
    element(
      figure,
      [],
      [
        element(image, [xlink:type=simple, xlink:href=RelativeImageURI], []),
        element(caption, [], Caption2)
      ]
    )
  | Contents2
  ]
):-
  !,
  dbnl_markup(Options, Caption1, Caption2),

  % Store the image locally.
  memberchk(src=RelativeImageURI, IMG_Attributes),
  absolute_file_name(file(RelativeImageURI), ImageFile, []),
  option(uri(BaseURI), Options),
  uri_resolve(RelativeImageURI, BaseURI, AbsoluteImageURI),
  uri_to_file(AbsoluteImageURI, ImageFile),

  % Also add the image as RDF data.
  rdf_bnode(BNode2),
  option(graph(Graph), Options),
  rdfs_assert_individual(BNode2, dbnl:'Image', Graph),
  rdf_assert_datatype(BNode2, dbnl:file, file, ImageFile, Graph),
  dom_to_xml(dbnl, Caption2, XML_Caption),
  rdf_assert_xml_literal(BNode2, dbnl:caption, XML_Caption, Graph),
  option(bnode(BNode1), Options),
  rdf_assert(BNode1, dbnl:image, BNode2, Graph),

  dbnl_markup(Options, Contents1, Contents2).
% Image without caption.
dbnl_markup(
  Options,
  [element(img, IMG_Attributes, []) | Contents1],
  [
    element(
      figure,
      [],
      [element(image, [xlink:type=simple, xlink:href=RelativeImageURI], [])]
    )
  | Contents2
  ]
):-
  !,
  % Store the image locally.
  memberchk(src=RelativeImageURI, IMG_Attributes),
  absolute_file_name(file(RelativeImageURI), ImageFile, []),
  option(uri(BaseURI), Options),
  uri_resolve(RelativeImageURI, BaseURI, AbsoluteImageURI),
  uri_to_file(AbsoluteImageURI, ImageFile),

  % Also add the image as RDF data.
  rdf_bnode(BNode2),
  option(graph(Graph), Options),
  rdfs_assert_individual(BNode2, dbnl:'Image', Graph),
  rdf_assert_datatype(BNode2, dbnl:file, file, ImageFile, Graph),
  option(bnode(BNode1), Options),
  rdf_assert(BNode1, dbnl:image, BNode2, Graph),

  dbnl_markup(Options, Contents1, Contents2).
% Footnote.
dbnl_markup(
  Options,
  [element(a, Attributes, [element(span, _, [NoteName])]) | Contents1],
  [element(footnote, [name=NoteName], Note2) | Contents2]
):-
  !,
  memberchk(href=NoteIndex1, Attributes),
  strip([' ','#'], NoteIndex1, NoteIndex2),
  option(notes(Notes), Options),
  memberchk(NoteIndex2-Note1, Notes),
  dbnl_markup(Options, Note1, Note2),
  dbnl_markup(Options, Contents1, Contents2).
% A paragraph of text.
dbnl_markup(
  Options,
  [element(p, _, P_Contents1) | Contents1],
  Contents3
):-
  % We skip some paragraphs, since this would needlessly clutter the
  % XML structure in some cases.
  (
    % 1. Skip paragraphs with no content.
    P_Contents1 = [Atom],
    strip([' '], Atom, '')
  ->
    Contents3 = Contents2
  ;
    % 2. Skip paragraphs that only contain figures.
    forall(
      member(Member, P_Contents1),
      (
        Member = element(Element, _Attributes, _Content),
        member(Element, [br,img])
      )
    )
  ->
    dbnl_markup(Options, P_Contents1, P_Contents2),
    dbnl_markup(Options, Contents1, Contents2),
    append(P_Contents2, Contents2, Contents3)
  ;
    % Other paragraphs are included.
    dbnl_markup(Options, P_Contents1, P_Contents2),
    dbnl_markup(Options, Contents1, Contents2),
    Contents3 = [element(paragraph, [], P_Contents2) | Contents2]
  ).
% SPAN class=topo ???
dbnl_markup(
  Options,
  [element(span, [class=topo], SPAN_Contents) | Contents1],
  [element(topic, [], Topic_Contents) | Contents2]
):-
  !,
  dbnl_markup(Options, SPAN_Contents, Topic_Contents),
  dbnl_markup(Options, Contents1, Contents2).
% A blockquote.
dbnl_markup(
  Options,
  [element(blockquote, [], Blockquote_Contents) | Contents1],
  [element(quote, [], Quote_Contents) | Contents2]
):-
  !,
  dbnl_markup(Options, Blockquote_Contents, Quote_Contents),
  dbnl_markup(Options, Contents1, Contents2).
% A piece of italic text.
dbnl_markup(
  Options,
  [element(i, [], I_Contents) | Contents1],
  [element(emphasis, [], Emphasis_Contents) | Contents2]
):-
  !,
  dbnl_markup(Options, I_Contents, Emphasis_Contents),
  dbnl_markup(Options, Contents1, Contents2).
% Superscript.
dbnl_markup(
  Options,
  [element(sup, [], SUP_Contents) | Contents1],
  [element(superscript, [], Superscript_Contents) | Contents2]
):-
  !,
  dbnl_markup(Options, SUP_Contents, Superscript_Contents),
  dbnl_markup(Options, Contents1, Contents2).
% Table.
dbnl_markup(
  Options,
  [element(table, _, [TBODY]) | Contents1],
  [element(ilines, [], IndexedLines) | Contents2]
):-
  !,
  findall(
    TR,
    xpath(TBODY, tr, TR),
    TRs
  ),
  dbnl_indexed_lines(Options, TRs, IndexedLines),
  dbnl_markup(Options, Contents1, Contents2).
% Some stuff is simply skipped...
dbnl_markup(Options, [element(a, Attributes, _) | Contents1], Contents2):-
  \+ member(href=_, Attributes),
  !,
  dbnl_markup(Options, Contents1, Contents2).
% Some stuff is simply skipped...
dbnl_markup(Options, [element(br, _, _) | Contents1], Contents2):-
  !,
  dbnl_markup(Options, Contents1, Contents2).
% Some stuff is simply skipped...
dbnl_markup(Options, [element(interp, _, _) | Contents1], Contents2):-
  !,
  dbnl_markup(Options, Contents1, Contents2).
% A piece of plain text.
dbnl_markup(Options, [Text | Contents1], [Text | Contents2]):-
  atom(Text),
  !,
  dbnl_markup(Options, Contents1, Contents2).
% Debug on elements that are not yet treated.
dbnl_markup(Options, [Element | Contents1], Contents2):-
  format(user_output, '~w\n', [Element]), %DEB
  dbnl_markup(Options, Contents1, Contents2).

dbnl_markup_test:-
  URI = 'http://www.dbnl.org/tekst/_aan001aanm01_01/_aan001aanm01_01_0011.php',
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents),
  dbnl_dom_notes(DOM, Notes),
  rdf_bnode(BNode),
  dbnl_markup(
    [bnode(BNode), graph(test), notes(Notes), title(URI), uri(URI)],
    Contents,
    ResultDOM
  ),
  absolute_file_name(project(deb), File, [file_type(xml)]),
  xml_current_namespace(xlink, XLinkNamespace),
  dom_to_xml_file(dbnl, ResultDOM, File, [nsmap([xlink=XLinkNamespace])]).

