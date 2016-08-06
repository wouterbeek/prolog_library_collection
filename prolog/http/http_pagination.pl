:- module(
  http_pagination,
  [
    pagination_next_iri/2, % +Result, -Next
    pagination_prev_iri/2  % +Result, -Prev
  ]
).

/** <module> HTTP pagination

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(dict_ext)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(pagination)).
:- use_module(library(uri)).





%! pagination_next_iri(+Result, -Iri) is det.
%
% Fails silently when there is no next page.

pagination_next_iri(Result, Iri) :-
  pagination_next(Result, Next),
  pagination_iri(Result, Next, Iri).



%! pagination_prev_iri(+Result, -Iri) is det.
%
% Fails silently when there is no previous page.

pagination_prev_iri(Result, Iri) :-
  pagination_prev(Result, Prev),
  pagination_iri(Result, Prev, Iri).





% HELPERS %

%! pagination_iri(+Result, +Page, -Iri) is det.

pagination_iri(Result, Page, Iri) :-
  get_dict(query, Result, Query0, []),
  uri_query_components(Query, [page(Page)|Query0]),
  iri_change_comp(Result.iri, query, Query, Iri).
