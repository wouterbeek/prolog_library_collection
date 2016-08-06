:- module(
  http_pagination,
  [
    http_pagination_link_first/1, % +Result
    http_pagination_link_last/1,  % +Result
    http_pagination_link_next/1,  % +Result
    http_pagination_link_prev/1,  % +Result
    pagination_first_iri/2,       % +Result, -First
    pagination_last_iri/2,        % +Result, -Last
    pagination_next_iri/2,        % +Result, -Next
    pagination_prev_iri/2         % +Result, -Prev
  ]
).

/** <module> HTTP pagination

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(dict_ext)).
:- use_module(library(http/http_path)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(pagination)).
:- use_module(library(uri)).





%! http_pagination_link_first(+Result) is semidet.
%
% Fails silently if there is not a signle page.

http_pagination_link_first(Result) :-
  pagination_page_iri(Result, 1, First),
  http_absolute_uri(First, Target),
  format('Link: <~a>; rel="first"~n', [Target]).



%! http_pagination_link_last(+Result) is semidet.
%
% Fails silently if there is not a single page.

http_pagination_link_last(Result) :-
  pagination_page_iri(Result, 1, Last),
  http_absolute_uri(Last, Target),
  format('Link: <~a>; rel="last"~n', [Target]).



%! http_pagination_link_next(+Result) is semidet.
%
% Fails silently if there is not next page.

http_pagination_link_next(Result) :-
  pagination_next_iri(Result, Next),
  http_absolute_uri(Next, Target),
  format('Link: <~a>; rel="next"~n', [Target]).



%! http_pagination_link_prev(+Result) is semidet.
%
% Fails silently if there is no previous page.

http_pagination_link_prev(Result) :-
  pagination_prev_iri(Result, Prev),
  http_absolute_uri(Prev, Target),
  format('Link: <~a>; rel="prev"~n', [Target]).



%! pagination_first_iri(+Result, -Iri) is semidet.
%
% Fails silently when there is not a single page.

pagination_first_iri(Result, Iri) :-
  pagination_first(Result, First),
  pagination_page_iri(Result, First, Iri).



%! pagination_last_iri(+Result, -Iri) is semidet.
%
% Fails silently when there is not a single page.

pagination_last_iri(Result, Iri) :-
  pagination_last(Result, Last),
  pagination_page_iri(Result, Last, Iri).



%! pagination_next_iri(+Result, -Iri) is semidet.
%
% Fails silently when there is no next page.

pagination_next_iri(Result, Iri) :-
  pagination_next(Result, Next),
  pagination_page_iri(Result, Next, Iri).



%! pagination_prev_iri(+Result, -Iri) is semidet.
%
% Fails silently when there is no previous page.

pagination_prev_iri(Result, Iri) :-
  pagination_prev(Result, Prev),
  pagination_page_iri(Result, Prev, Iri).





% HELPERS %

%! pagination_page_iri(+Result, +Page, -Iri) is det.

pagination_page_iri(Result, Page, Iri) :-
  get_dict(query, Result, Query0, []),
  uri_query_components(Query, [page(Page)|Query0]),
  iri_change_comp(Result.iri, query, Query, Iri).
