:- module(
  http_pagination,
  [
    http_pagination_header/1, % +Result
    http_pagination_iri/3,    % +Result, ?Rel, -Iri
  ]
).
:- reexport(library(pagination/pagination)).

/** <module> HTTP pagination

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(uri)).





%! http_pagination_header(+Result) is semidet.

http_pagination_header(Result) :-
  http_pagination_iris(Result, Pairs),
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

http_pagination_header_comp0(Rel-Iri) :-
  format('<~a>; rel="~a"', [Iri,Rel]).



%! http_pagination_iri(+Result, +Rel, -Iri) is semidet.
%! http_pagination_iri(+Result, -Rel, -Iri) is nondet.

http_pagination_iri(Result, Rel, Iri) :-
  pagination_page(Result, Rel, Page),
  dict_get(query, Result, [], QueryComps),
  uri_query_components(Query, [page(Page)|QueryComps]),
  uri_components(Result.iri, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)).



%! http_pagination_iris(+Result, -Pairs) is det.
%
% Returns the Page for the given pagination Result.
%
% @param Pairs are of the form `Rel-Iri`.

http_pagination_iris(Result, Pairs) :-
  findall(Rel-Iri, http_pagination_iri(Result, Rel, Iri), Pairs).
