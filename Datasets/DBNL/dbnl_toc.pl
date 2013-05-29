:- module(
  dbnl_toc,
  [
    dbnl_toc/3 % +Graph:atom
               % +TOC_URI:uri
               % +Contents:dom
  ]
).

/** <module> DBNL TOC

Predicates for asserting a table of contents text from the DBNL.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_bibliography)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_text)).
:- use_module(library(lists)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).
:- use_module(xml(xlink)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').
:- xml_register_namespace(xlink, 'http://www.w3.org/1999/xlink').



%! dbnl_toc(+Graph:atom, +TOC:uri, +Contents:dom) is det.
% Processes the given DOM which represents the list of contents
% of the given title.
%
% The list of contents is asserted as an RDF list consisting of blank nodes
% for each chapter.

dbnl_toc(Graph, TOC, Contents):-
  dbnl_toc(Graph, TOC, Subtexts, Contents),
  rdf_assert_list(Subtexts, RDF_List, Graph),
  rdf(Title, dbnl:text, TOC, Graph),
  rdf_assert(Title, dbnl:toc, RDF_List, Graph).

%! dbnl_toc(
%!   +Graph:atom,
%!   +TOC:uri,
%!   +History:list(bnode),
%!   +Contents:dom
%! ) is det.

dbnl_toc(_Graph, _TOC, [], []):-
  !.
% Process the contents of paragraphs.
% I have seen paragraphs containing one and paragraphs containing two links,
% so the case for one nested link inside a pragraph did not work.
dbnl_toc(Graph, TOC, Subtexts, [element(p, [], Content) | Contents]):-
  dbnl_toc(Graph, TOC, Subtexts1, Content),
  dbnl_toc(Graph, TOC, Subtexts2, Contents),
  append(Subtexts1, Subtexts2, Subtexts).
% A link to a chapter.
dbnl_toc(Graph, TOC, [Subtext | Subtexts], [element(a, Attributes, [SubtextName]) | Contents]):-
  memberchk(href=RelativeURI, Attributes),
  rdf(TOC, dbnl:original_page, BaseURI, Graph),
  % @tbd uri_resolve/3 cannot handle this?!
  dbnl_uri_resolve(RelativeURI, BaseURI, AbsoluteURI),

  % Process the chapter's contents.
  (
    SubtextName == ['Bibliografie']
  ->
    dbnl_bibliography(Graph, AbsoluteURI, Subtext)
  ;
    dbnl_text(Graph, TOC, AbsoluteURI, Subtext)
  ),
  rdfs_assert_label(Subtext, SubtextName, Graph),
  rdf_assert(TOC, dbnl:chapter, Subtext),
  dbnl_toc(Graph, TOC, Subtexts, Contents).
% Debug.
dbnl_toc(Graph, TOC, Subtexts, [Content | Contents]):-
  gtrace, %DEB
  format(user_output, '~w\n', [Content]),
  dbnl_toc(Graph, TOC, Subtexts, Contents).
