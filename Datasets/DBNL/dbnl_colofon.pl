:- module(
  dbnl_colofon,
  [
    dbnl_colofon/3 % +Graph:atom
                   % +Title:uri
                   % +URI:uri
  ]
).

/** <module> DBNL COLOFON

Scrapes a colofon page from the DBNL.

@author Wouter Beek
@tbd Just a stub!
@version 2013/05
*/

:- use_module(dbnl(dbnl_generic)).



%! dbnl_colofon(+Graph:atom, +Title:uri, +URI:uri) is det.
% Asserts the contents that are found in the given colofon URI
% for the given title.
%
% @tbd Extract the various fields from the DOM.

dbnl_colofon(_Graph, _Title, URI):-
  dbnl_uri_to_html(URI, DOM),
  dbnl_dom_center(DOM, Contents),
  write(Contents).

