:- module(
  dbnl_secondary_summary,
  [
    dbnl_secondary//2, % +Graph:atom
                       % +Title:uri
    dbnl_summary//2 % +Graph:atom
                    % +Title:uri
  ]
).

/** <module> DBNL SECONDARY

Predicates for processing lists of secondary literature,
as they occur in DBNL title pages.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_db)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_generic)).
:- use_module(dbnl(dbnl_text)).
:- use_module(dcg(dcg_ascii)).
:- use_module(dcg(dcg_generic)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



dbnl_secondary(Graph, Title) -->
  dbnl_secondary_summary(Graph, Title, secondary).

%! dbnl_secondary_summary(
%!   +Graph:atom,
%!   +Title:uri,
%!   +Type:oneof([secondary,summary]),
%!   +Contents:dom
%! ) is det.

dbnl_secondary_summary(Graph, Title, Type) -->
  [element(dt, [], C)],
  {phrase(dbnl_secondary_summary(Graph, Title, Type), C)}.
dbnl_secondary_summary(Graph, Title, Type) -->
  [AuthorName, element(a, Attrs, [TitleName]), Atom],
  {
    memberchk(href=RelativeURI, Attrs),
    dbnl_uri_resolve(RelativeURI, AbsoluteURI),
    dbnl_text(Graph, Title, AbsoluteURI, Text),
    rdf_global_id(dbnl:Type, Predicate),
    rdf_assert(Title, Predicate, Text, Graph),
    rdf_assert_datatype(Text, dbnl:author, string, AuthorName, Graph),
    rdf_assert_datatype(Text, dbnl:title, string, TitleName, Graph),
    rdf_assert(Text, dbnl:original_page, AbsoluteURI, Graph),
    atom_codes(Atom, Codes),
    phrase(dbnl_secondary_summary0(Graph, Title), Codes)
  }.

dbnl_journal(Lang, Name) -->
  pre(Lang), colon, blank,
  string_until(",", Codes),
  {atom_codes(Name, Codes)}.

dbnl_secondary_summary0(Graph, Title) -->
  dbnl_journal(Lang, JournalName), comma, blank,
  {rdf_assert_literal(Title, dbnl:venue, JournalName, Graph)},
  dbnl_year(Lang, Year),
  {dbnl_assert_year(Graph, Title, Year)}.
dbnl_secondary_summary0(_Graph, _Title, R, []):-
  gtrace, %DEB
  format(user_output, '~w\n', [R]).

dbnl_summary(Graph, Title) -->
  dbnl_secondary_summary(Graph, Title, summary).

pre(nl) --> "In".
pre(nl) --> "in".

