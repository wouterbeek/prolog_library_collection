:- module(
  dbnl_primary,
  [
    dbnl_primary/3 % +Graph:atom
                   % +Title:uri
                   % +URI:uri
  ]
).

/** <module> DBNL PRIMARY TEXT

Predicates for primary text links.

@author Wouter Beek
@version 2013/05
*/

:- use_module(dbnl(dbnl_text)).
:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(dbnl, 'http://www.dbnl.org/').



%! dbnl_primary(+Graph:atom, +Title:uri, +URI:uri) is det.
% Process the text that is located at the given URI.
%
% There are three types of pages that may be accessible from the text page:
%   1. Colofon page.
%   2. Downloads page.
%   3. Index page.

dbnl_primary(Graph, Title, URI):-
  dbnl_primary0(Graph, Title, URI, PrimaryText),
  rdf_assert(Title, dbnl:primary, PrimaryText, Graph).

dbnl_primary0(Graph, _Title, URI, PrimaryText):-
  rdf(PrimaryText, dbnl:original_page, URI, Graph),
  !.
dbnl_primary0(Graph, Title, URI, PrimaryText):-
  dbnl_text(Graph, Title, URI, PrimaryText).

