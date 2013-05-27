:- module(
  dbnl_generic,
  [
    dbnl_authority/1, % -Authority:atom
    dbnl_base_uri/1, % -Base:uri
    dbnl_debug/1, % +URI:uri
    dbnl_dom_to_center/2, % +Page:dom
                          % -Center:dom
    dbnl_dom_to_footnotes/2, % +Page:dom
                             % -Notes:pair(atom,dom))
    dbnl_scheme/1, % -Scheme:atom
    dbnl_uri_resolve/2, % +Relative:uri
                        % -Absolute:uri
    dbnl_uri_to_html/2 % +URI:uri
		       % -HTML:dom
  ]
).

/** <module> DBNL GENERIC

Generic predicates for scraping the DBNL.

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(lists)).
:- use_module(library(uri)).
:- use_module(library(xpath)).
:- use_module(library(www_browser)).



%! dbnl_authority(-Authority:atom) is det.
% Returns the authority of the DBNL.

dbnl_authority('www.dbnl.org').

%! dbnl_base_uri(-BaseURI:uri) is det.
% Returns the base URI for the DBNL.

dbnl_base_uri(BaseURI):-
  dbnl_scheme(Scheme),
  dbnl_authority(Authority),
  uri_components(BaseURI, uri_components(Scheme, Authority, '', '', '')).

dbnl_debug(URI1):-
  dbnl_uri_resolve(URI1, URI2),
  flag(deb, ID, ID + 1),
  (
    ID > 57
  ->
    once(www_open_url(URI2)),
    gtrace
  ;
    true
  ).

%! dbnl_dom_to_center(+DOM:dom, -Contents:dom) is det.

dbnl_dom_to_center(DOM, Contents):-
  findall(
    Contents,
    (
      xpath(DOM, //td(@id=text), TD),
      xpath(TD, div(content), Contents)
    ),
    Contentss
  ),
  append(Contentss, Contents).

%! dbnl_dom_to_footnotes(+DOM:dom, -Notes:pair(atom,dom)) is det.

dbnl_dom_to_footnotes(DOM, Notes):-
  findall(
    NoteIndex-Contents,
    (
      xpath(DOM, //div(@class=note), DIV),
      DIV = element(div, _, [element(a, Attributes, _) | Contents]),
      memberchk(name=NoteIndex, Attributes)
    ),
    Notes
  ).

%! dbnl_scheme(-Scheme:atom) is det.

dbnl_scheme(http).

%! dbnl_uri_resolve(+Relative:uri, -Absolute:uri) is det.
% Resolve a relative URI into an absolute URI.

dbnl_uri_resolve(Absolute, Absolute):-
  uri_is_global(Absolute),
  !.
dbnl_uri_resolve(Relative, Absolute):-
  dbnl_base_uri(Base),
  uri_resolve(Relative, Base, Absolute).

dbnl_uri_to_html(URI1, DOM):-
  dbnl_uri_resolve(URI1, URI2),
  dbnl_debug(URI2),
  uri_to_html(URI2, DOM).

