:- module(
  http_pagination,
  [
    http_pagination_header/1, % +Page
    http_pagination_json/1,   % +Page
    http_pagination_link/3,   % +Page, +Relation, -Uri
    http_pagination_links/2   % +Page, -Links
  ]
).

/** <module> Support for HTTP pagination

*/

:- use_module(library(http/json)).
:- use_module(library(settings)).

:- use_module(library(dict)).
:- use_module(library(pagination)).
:- use_module(library(rest_server), []). % HTTP parameter hook
:- use_module(library(uri_ext)).

:- multifile
    http:param/2.

http:param(page, [
  default(1),
  description("The page number from the results set."),
  positive_integer
]).
http:param(page_size, [
  between(1, MaxPageSize),
  default(DefaultPageSize),
  description("The number of results per full result set page.")
]) :-
  setting(pagination:default_page_size, DefaultPageSize),
  setting(pagination:maximum_page_size, MaxPageSize).





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
  format("Content-Type: application/json; charset=UTF-8\n"),
  http_pagination_header(Page),
  nl,
  json_write_dict(current_output, Page.results).



%! http_pagination_link(+Page:dict, +Relation:atom, -Uri:atom) is semidet.
%! http_pagination_link(+Page:dict, -Relation:atom, -Uri:atom) is nondet.

http_pagination_link(Page, Relation, Uri) :-
  pagination_page(Page, Relation, PageNumber),
  dict_get(query, Page, [], Query1),
  (   dict_get(page_size, Page, PageSize)
  ->  Query2 = [page_size(PageSize)|Query1]
  ;   Query2 = Query1
  ),
  uri_comps(Page.uri, uri(Scheme,Auth,Segments,_,_)),
  uri_comps(Uri, uri(Scheme,Auth,Segments,[page(PageNumber)|Query2],_)).



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
