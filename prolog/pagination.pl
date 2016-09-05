:- module(
  pagination,
  [
    http_pagination_links/1,   % +Pagination
    pagination/3,              % +Pattern, :Goal_0, -Pagination
    pagination/4,              % +Pattern, :Goal_0, +PageOpts, -Pagination
    pagination_at_end/1,       % +Pagination
    pagination_empty/2,        % +PageOpts, -Pagination
    pagination_init_options/4, % +PageOpts1, -StartPage, -PageSize, -PageOpts2
    pagination_iri/3,          % +Pagination, ?Rel, -Iri
    pagination_page/3,         % +Pagination, ?Rel, -Page
    pagination_range/2,        % +Pagination, -Range
    pagination_result/2,       % +Pagination, :Goal_1
    pagination_total/4         % :AllGoal_2, :SomeGoal_2, +PageOpts, -Pagination
  ]
).

/** <module> Pagination

Generic support for pagination.

```prolog
?- pagination(N, between(1, 1000000, N), _{page: 856}, Paginations).
Paginations = _G120{number_of_results:20, page:856, page_size:20, results:[17101, 17102, 17103, 17104, 17105, 17106, 17107, 17108|...]} ;
Paginations = _G147{number_of_results:20, page:857, page_size:20, results:[17121, 17122, 17123, 17124, 17125, 17126, 17127, 17128|...]} ;
Paginations = _G147{number_of_results:20, page:858, page_size:20, results:[17141, 17142, 17143, 17144, 17145, 17146, 17147, 17148|...]} 
```

@author Wouter Beek
@verson 2016/03, 2016/05-2016/09
*/

:- use_module(library(dict_ext)).
:- use_module(library(http/http_path)).
:- use_module(library(iri/iri_ext)).
:- use_module(library(print_ext)).
:- use_module(library(settings)).
:- use_module(library(uri)).

:- meta_predicate
    pagination(+, 0, -),
    pagination(+, 0, +, -),
    pagination_result(+, 1),
    pagination_total(2, 2, +, -).

:- setting(
     def_page_size,
     positive_integer,
     20,
     "The default number of results per page."
   ).





%! http_pagination_links(+Pagination) is semidet.

http_pagination_links(Pagination) :-
  pagination_iris(Pagination, Rels, Iris),
  format("Link: "),
  http_pagination_links0(Rels, Iris),
  nl.


http_pagination_links0([], []) :- !.
http_pagination_links0([Rel], [Iri]) :- !,
  http_pagination_link0(Rel, Iri).
http_pagination_links0([Rel|Rels], [Iri|Iris]) :- !,
  http_pagination_link0(Rel, Iri),
  format(", "),
  http_pagination_links0(Rels, Iris).


http_pagination_link0(Rel, Iri) :-
  format('<~a>; rel="~a"', [Iri,Rel]).



%! pagination(+Pattern, :Goal_0, -Pagination) is nondet.
%! pagination(+Pattern, :Goal_0, +PageOpts, -Pagination) is nondet.
%
% The following options are supported:
%
%   - page_size(+nonneg) Default is 100.
%
%   - page(+nonneg) Default is 1.
%
% The following keys are in Pagination:
%
%   - number_of_results(nonneg)  The page_size or less.
%
%   - page(positive_integer)
%
%   - page_size(positive_integer)
%
%   - results(nonneg)

pagination(Pattern, Goal_0, Pagination) :-
  pagination(Pattern, Goal_0, _{}, Pagination).


pagination(Pattern, Goal_0, PageOpts1, Pagination2) :-
  pagination_init_options(PageOpts1, StartPage, PageSize, PageOpts2),
  put_dict(page0, PageOpts2, 0, PageOpts3),
  findnsols(PageSize, Pattern, Goal_0, Results),
  dict_inc(page0, PageOpts3),
  length(Results, NumResults),
  (   % No more results.
      NumResults =:= 0
  ->  true
  ;   % Skip pages that are before the start page.
      PageOpts3.page0 >= StartPage
  ->  true
  ;   false
  ), !,
  Pagination1 = _{
    number_of_results: NumResults,
    page: PageOpts3.page0,
    page_size: PageSize,
    results: Results
  },
  merge_dicts(PageOpts3, Pagination1, Pagination2).
pagination(_, _, PageOpts, Pagination) :-
  pagination_empty(PageOpts, Pagination).



%! pagination_at_end(+Pagination) is semidet.
%
% Succeeds if pagination Paginations are at the last page.
%
% @note Since we do not know the total number of results, the last
%       page may be empty.

pagination_at_end(Pagination) :-
  Pagination.number_of_results < Pagination.page_size.



%! pagination_empty(+PageOpts, -Pagination) is det.
%
% Returns a pagination Pagination dictionary with empty results.

pagination_empty(PageOpts, Pagination) :-
  pagination(_, fail, PageOpts, Pagination).



%! pagination_iri(+Pagination, +Rel, -Iri) is semidet.
%! pagination_iri(+Pagination, -Rel, -Iri) is nondet.

pagination_iri(Pagination, Rel, Iri) :-
  pagination_page(Pagination, Rel, Page),
  dict_get(query, Pagination, [], QueryComps),
  uri_query_components(Query, [page(Page)|QueryComps]),
  uri_components(Pagination.iri, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)).



%! pagination_init_options(
%!   +PageOpts1,
%!   -StartPage,
%!   -PageSize,
%!   -PageOpts2
%! ) is det.

pagination_init_options(PageOpts1, StartPage, PageSize, PageOpts3) :-
  mod_dict(page, PageOpts1, 1, StartPage, PageOpts2),
  setting(def_page_size, DefPageSize),
  mod_dict(page_size, PageOpts2, DefPageSize, PageSize, PageOpts3).



%! pagination_iris(+Pagination, -Rels, -Iris) is det.
%
% Returns the Page for the given pagination Pagination.  Fails
% silently when there are no pages with relation Rel.

pagination_iris(Pagination, Rels, Iris) :-
  findall(Rel-Iri, pagination_iri(Pagination, Rel, Iri), Pairs),
  pairs_keys_values(Pairs, Rels, Iris).



%! pagination_page(+Pagination, +Rel, -Iri) is semidet.
%! pagination_page(+Pagination, -Rel, -Iri) is nondet.

pagination_page(Pagination, first, 1) :-
  Pagination.number_of_results > 0.
pagination_page(Pagination, last, Page) :-
  get_dict(total_number_of_results, Pagination, TotalNumResults),
  Page is ceil(TotalNumResults / Pagination.page_size).
pagination_page(Pagination, next, Page) :-
  \+ pagination_at_end(Pagination),
  Page is Pagination.page + 1.
pagination_page(Pagination, prev, Page) :-
  Page is Pagination.page - 1,
  Page > 0.



%! pagination_range(+Pagination, -Range) is det.
%
% Returns the range spanned by the given pagination Pagination.

pagination_range(Pagination, 0-0) :-
  Pagination.number_of_results =:= 0, !.
pagination_range(Pagination, Low-High) :-
  Low is (Pagination.page - 1) * Pagination.page_size + 1,
  High is Low + Pagination.number_of_results - 1.



%! pagination_result(+Pagination, :Goal_1) is det.

pagination_result(Pagination, Goal_1) :-
  call(Goal_1, Pagination.results),
  nl,
  Opts = [fg(green)],
  (   Pagination.results == []
  ->  ansi_format(Opts, "No results for query.~n", [])
  ;   Low is (Pagination.page - 1) * Pagination.page_size,
      LowDisplay is Low + 1,
      High is Low + Pagination.number_of_results,
      ansi_format(
        Opts,
        "Showing results ~D -- ~D for query.~n",
        [LowDisplay,High]
      )
  ).



%! pagination_total(:AllGoal_2, :SomeGoal_2, +PageOpts, -Pagination) is nondet.
%
% The same options as pagination/4.
%
% The same keys as Pagination of pagination/4, plus:
%
%   - total_number_of_results(+nonneg)

pagination_total(AllGoal_2, SomeGoal_2, PageOpts1, Pagination2) :-
  pagination_init_options(PageOpts1, StartPage, PageSize, PageOpts2),
  put_dict(page0, PageOpts2, 0, PageOpts3),
  call(AllGoal_2, AllResults, TotalNumResults),
  findnsols(
    PageSize,
    SomeResult,
    call(SomeGoal_2, AllResults, SomeResult),
    Results
  ),
  dict_inc(page0, PageOpts3),
  length(Results, NumResults),
  (   % No more results.
      NumResults =:= 0
  ->  true
  ;   % Skip pages that are before the start page.
      PageOpts3.page0 >= StartPage
  ->  true
  ;   false
  ),
  Pagination1 = _{
    number_of_results: NumResults,
    page: PageOpts3.page0,
    page_size: PageSize,
    results: Results,
    total_number_of_results: TotalNumResults
  },
  merge_dicts(PageOpts3, Pagination1, Pagination2).
