:- module(
  http_pagination,
  [
    http_pagination_header/1, % +Result
    http_pagination_uri/3,    % +Result, ?Rel, -Uri
    http_pagination_uris/2    % +Result, -Pairs
  ]
).
:- reexport(library(pagination/pagination)).

/** <module> HTTP pagination

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(dict_ext)).
:- use_module(library(uri)).





%! http_pagination_header(+Result) is semidet.

http_pagination_header(Result) :-
  http_pagination_uris(Result, Pairs),
  format("Link: "),
  http_pagination_header0(Pairs),
  nl.

http_pagination_header0([]) :- !.
http_pagination_header0([H]) :- !,
  http_pagination_header_comp0(H).
http_pagination_header0([H|T]) :- !,
  http_pagination_header_comp0(H),
  format(", "),
  http_pagination_header0(T).

http_pagination_header_comp0(Rel-Uri) :-
  format('<~a>; rel="~a"', [Uri,Rel]).



%! http_pagination_uri(+Result, +Rel, -Uri) is semidet.
%! http_pagination_uri(+Result, -Rel, -Uri) is nondet.

http_pagination_uri(Result, Rel, Uri) :-
  pagination_page(Result, Rel, Page),
  get_dict(query, Result, [], QueryComps),
  uri_query_components(Query, [page(Page)|QueryComps]),
  uri_components(Result.uri, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Uri, uri_components(Scheme,Auth,Path,Query,_)).



%! http_pagination_uris(+Result, -Pairs) is det.
%
% Returns the Page for the given pagination Result.
%
% @param Pairs are of the form `Rel-Uri`.

http_pagination_uris(Result, Pairs) :-
  findall(Rel-Uri, http_pagination_uri(Result, Rel, Uri), Pairs).
