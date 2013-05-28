:- module(
  dbnl_toc,
  [
    dbnl_toc/4 % +Graph:atom
               % +Text:uri
               % +Base:uri
               % +TOC:dom
  ]
).

/** <module> DBNL TOC

Predicates for asserting a table of contents text from the DBNL.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_bibliography)).
:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_markup)).
:- use_module(generics(atom_ext)).
:- use_module(library(option)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xlink)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').
:- xml_register_namespace(xlink, 'http://www.w3.org/1999/xlink').



%! dbnl_subtext(+Options:list(nvpair), +URI:uri, -XML:dom) is det.
% Returns the XML DOM for the given chapter link.
%
% Options:
%   * bnode
%   * graph
%   * notes
%   * title
%   * uri

dbnl_subtext(
  Options1,
  URI,
  [element(text, [xmlns:xlink=XLinkNamespace], SubtextDOM)]
):-
  xml_current_namespace(xlink, XLinkNamespace),
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents),
  dbnl_dom_notes(DOM, Notes),
  merge_options([notes(Notes), uri(URI)], Options1, Options2),
  dbnl_markup(Options2, Contents, SubtextDOM).

%! dbnl_toc(
%!   +Graph:atom,
%!   +Text:uri,
%!   +Base:uri,
%!   +Contents:dom
%! ) is det.
% Processes the given DOM which represents the list of contents
% of the given title.
%
% The list of contents is asserted as an RDF list consisting of blank nodes
% for each chapter.

dbnl_toc(Graph, Text, BaseURI, Contents):-
  rdfs_assert_individual(Text, dbnl:'TOC', Graph),
  dbnl_toc(Graph, Text, BaseURI, [], Contents).

%! dbnl_toc(
%!   +Graph:atom,
%!   +Text:uri,
%!   +Base:uri,
%!   +History:list(bnode),
%!   +Contents:dom
%! ) is det.

dbnl_toc(Graph, Text, _BaseURI, Subtexts, []):-
  rdf_assert_list(Subtexts, RDF_List, Graph),
  rdf(Title, dbnl:text, Text, Graph),
  rdf_assert(Title, dbnl:toc, RDF_List, Graph),
  !.
% A link to a chapter.
dbnl_toc(
  Graph,
  Text,
  BaseURI,
  Subtexts,
  [element(p, [], [element(a, Attributes, Content1)]) | Contents]
):-
  memberchk(href=RelativeURI, Attributes),
  % @tbd uri_resolve/3 cannot handle this?!
  atomic_list_concat([BaseURI, '/', RelativeURI], AbsoluteURI),
  dbnl_assert_text(Graph, AbsoluteURI, Subtext),
  
  Options = [graph(Graph), title(Text), uri(AbsoluteURI)],
  dbnl_markup(Options, Content1, Content2),
  rdfs_assert_label(Subtext, Content2, Graph),

  % Process the chapter's contents.
  (
    Content2 == ['Bibliografie']
  ->
    dbnl_bibliography(Options, AbsoluteURI)
  ;
    dbnl_subtext(Options, AbsoluteURI, SubtextDOM),
    
    % Write the contents to an XML file.
    file_name_extension(Base, _Extension, RelativeURI),
    absolute_file_name(file(Base), XML_File, [file_type(xml)]),
    xml_current_namespace(xlink, XLinkNamespace),
    dom_to_xml_file(
      dbnl,
      SubtextDOM,
      XML_File,
      [nsmap([xlink=XLinkNamespace])]
    ),
    rdf_assert_datatype(Subtext, dbnl:content, file, XML_File, Graph)
  ),
  dbnl_toc(Graph, Text, BaseURI, [Subtext | Subtexts], Contents).

