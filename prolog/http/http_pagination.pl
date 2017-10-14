:- module(
  http_pagination,
  [
    http_pagination_header/1, % +Page
    http_pagination_json/1,   % +Page
    http_pagination_link/3,   % +Page, +Relation, -Uri
    http_pagination_links/2   % +Page, -Links
  ]
).

/** <module> HTTP support for pagination

@author Wouter Beek
@version 2017/05-2017/10
*/

:- use_module(library(dict_ext)).
:- use_module(library(http/json)).
:- use_module(library(pagination)).
:- use_module(library(uri/uri_ext)).





%! http_pagination_header(+Page:dict) is semidet.

http_pagination_header(Page) :-
  http_pagination_links(Page, Links),
  format("Link: "),
  http_pagination_header_values(Links),
  nl.

http_pagination_header_values([]) :- !.
http_pagination_header_values([H]) :- !,
  http_pagination_header_value(H).
http_pagination_header_values([H|T]) :- !,
  http_pagination_header_value(H),
  format(", "),
  http_pagination_header_values(T).

http_pagination_header_value(Relation-Uri) :-
  format('<~a>; rel="~a"', [Uri,Relation]).



%! http_pagination_json(+Page:dict) is det.

http_pagination_json(Page) :-
  format("Content-Type: application/json\n"),
  http_pagination_header(Page),
  nl,
  json_write_dict(current_output, Page.results).



%! http_pagination_link(+Page:dict, +Relation:atom, -Uri:atom) is semidet.
%! http_pagination_link(+Page:dict, -Relation:atom, -Uri:atom) is nondet.

http_pagination_link(Page, Relation, Uri) :-
  pagination_page(Page, Relation, PageNumber),
  dict_get(query, Page, [], QueryComps),
  uri_comps(Page.uri, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri, uri(Scheme,Auth,Segments,[page(PageNumber)|QueryComps],_)).



%! http_pagination_links(+Page:dict, -Links:list(pair(atom))) is det.
%
% Returns the Page for the given pagination Page.
%
% @param Pairs are of the form `Relation-Uri'.

http_pagination_links(Page, Links) :-
  findall(
    Relation-Uri,
    http_pagination_link(Page, Relation, Uri),
    Links
  ).
