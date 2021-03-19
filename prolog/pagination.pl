:- encoding(utf8).
:- module(
  pagination,
  [
    pagination/3,           % +Template, :Goal_0, -Page
    pagination/4,           % +Template, :Goal_0, +Options, -Page
    pagination/5,           % +Template, :Goal_0, :Estimate_1, +Options, -Page
    pagination_bulk/3,      % :Goal_1, +Options, -Page
    pagination_bulk/4,      % +Template, :Goal_0, +Options, -Page
    pagination_is_at_end/1, % +Result
    pagination_is_empty/1,  % +Result
    pagination_page/3,      % +Page, ?Relation, -PageNumber
    pagination_range/2      % +Page, -Range
  ]
).

/** <module> Pagination support

This module creates pages that group results.

```pl
?- pagination(N, between(1, 1000000, N), options{page: 856}, Result).
Result = _G120{number_of_results:20, page:856, page_size:20, results:[17101, 17102, 17103, 17104, 17105, 17106, 17107, 17108|...]} ;
Result = _G147{number_of_results:20, page:857, page_size:20, results:[17121, 17122, 17123, 17124, 17125, 17126, 17127, 17128|...]} ;
Result = _G147{number_of_results:20, page:858, page_size:20, results:[17141, 17142, 17143, 17144, 17145, 17146, 17147, 17148|...]}
```

*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(settings)).

:- use_module(library(dict)).
:- use_module(library(list_ext)).

:- meta_predicate
    pagination(?, 0, -),
    pagination(?, 0, +, -),
    pagination(?, 0, 1, +, -),
    pagination_bulk(1, +, -),
    pagination_bulk(?, 0, +, -).

:- setting(default_page_size, positive_integer, 10,
           "The default number of triples that is retreived in one request.").
:- setting(maximum_page_size, positive_integer, 100,
           "The maximum number of triples that can be retrieved in one request.").





%! empty_pagination(+Options:options, -Page:dict) is det.
%
% Returns a pagination Page dictionary with empty results.

empty_pagination(Options, Page) :-
  pagination(_, fail, Options, Page).



%! pagination(+Templ, :Goal_0, -Page:dict) is det.
%! pagination(+Templ, :Goal_0, +Options:options, -Page:dict) is det.
%! pagination(+Templ, :Goal_0, :Estimate_1, +Options:options, -Page:dict) is det.
%
% The following options are supported:
%
%   * page_number(+nonneg)
%
%     The page number of the Page in the implicit sequence.  The
%     default is 1.
%
%   * page_size(+positive_integer)
%
%     The number of results per (full) page.  The default is 10.
%
% @arg Page is a dictionary with the following keys:
%
%   * number_of_results(nonneg)
%
%     The number of results on the page.  This is either identical to
%     or less than the value of option page_size/1.
%
%   * page_number(positive_integer)
%
%     The page number of the page within the implicit sequence.
%
%   * page_size(positive_integer)
%
%     The number of results if the page were full.  This is identical
%     to the value of option page_size/1.
%
%   * results(list(term))
%
%     The results that are held by this Page.
%
%   * single_page(boolean)
%
%     A dirty hack to allow ‘pagination’ of one page, i.e., without
%     ‘next’ links.
%
%   * total_number_of_results(nonneg)
%
%     The total number of results, independent of pagination.  This is
%     only present when etimation goal Estimate_1 is passed.

pagination(Templ, Goal_0, Page) :-
  pagination(Templ, Goal_0, options{}, Page).


pagination(Templ, Goal_0, Options1, Page2) :-
  pagination_options(Options1, PageNumber, PageSize, Options2),
  Offset is PageSize * (PageNumber - 1),
  findall(Templ, limit(PageSize, offset(Offset, Goal_0)), Results),
  length(Results, NumResults),
  Page1 = options{
    number_of_results: NumResults,
    page_number: PageNumber,
    page_size: PageSize,
    results: Results,
    single_page: false
  },
  merge_dicts(Options2, Page1, Page2).
pagination(_, _, Options, Page) :-
  empty_pagination(Options, Page).


pagination(Templ, Goal_0, Estimate_1, Options1, Page2) :-
  pagination(Templ, Goal_0, Options1, Page1),
  call(Estimate_1, TotalNumResults),
  dict_put(total_number_of_results, Page1, TotalNumResults, Page2).



%! pagination_bulk(:Goal_1, +Options:options, -Page:dict) is nondet.
%! pagination_bulk(?Template, :Goal_0, +Options:options, -Page:dict) is nondet.

pagination_bulk(Goal_1, Options, Page) :-
  call(Goal_1, AllResults),
  pagination_bulk_(AllResults, Options, Page).


pagination_bulk(Templ, Goal_0, Options, Page) :-
  aggregate_all(set(Templ), Goal_0, AllResults),
  pagination_bulk_(AllResults, Options, Page).

pagination_bulk_(AllResults, Options1, Page2) :-
  pagination_options(Options1, StartPageNumber, PageSize, Options2),
  length(AllResults, TotalNumberOfResults),
  NumberOfPages is max(1, ceil(TotalNumberOfResults / PageSize)),
  must_be(between(1, NumberOfPages), StartPageNumber),
  between(StartPageNumber, NumberOfPages, PageNumber),
  SkipLength is PageSize * (PageNumber - 1),
  length(Skip, SkipLength),
  append(Skip, Rest, AllResults),
  list_truncate(Rest, PageSize, Results),
  length(Results, NumberOfResults),
  Page1 = options{
    number_of_results: NumberOfResults,
    page_number: PageNumber,
    page_size: PageSize,
    results: Results,
    single_page: false,
    total_number_of_results: TotalNumberOfResults
  },
  merge_dicts(Options2, Page1, Page2).



%! pagination_is_at_end(+Page:dict) is semidet.
%
% Succeeds if Page is the last page.
%
% Since we do not know the total number of results, the last page may
% be empty.

pagination_is_at_end(Page) :-
  options{single_page: true} :< Page, !.
pagination_is_at_end(Page) :-
  dict_get(total_number_of_results, Page, TotalNumResults),
  Page.page_number >= ceil(TotalNumResults / Page.page_size), !.
pagination_is_at_end(Page) :-
  Page.number_of_results < Page.page_size.



%! pagination_is_empty(+Page:dict) is semidet.

pagination_is_empty(Page) :-
  Page.number_of_results =:= 0.



%! pagination_options(+Options1:dict,
%!                    -StartPageNumber:positive_integer,
%!                    -PageSize:nonneg,
%!                    -Options2:dict) is det.

pagination_options(Options1, StartPageNumber, PageSize, Options3) :-
  dict_delete(page_number, Options1, 1, StartPageNumber, Options2),
  (   dict_delete(page_size, Options2, PageSize, Options3)
  ->  true
  ;   setting(default_page_size, PageSize),
      Options3 = Options2
  ).



%! pagination_page(+Page:dict, +Relation:atom, -PageNumber:positive_integer) is semidet.
%! pagination_page(+Page:dict, -Relation:atom, -PageNumber:positive_integer) is nondet.
%
% PageNumber is the first, last, next, and previous page number for
% Page.
%
% Fails silently when there is no page with relation Relation.
%
% @arg Relation is either `first', `last', `next', or `prev'.
%
% @arg PageNumber is a positive integer.

pagination_page(Page, first, 1) :-
  Page.number_of_results > 0.
pagination_page(Page, last, PageNumber) :-
  dict_get(total_number_of_results, Page, TotalNumResults),
  PageNumber is ceil(TotalNumResults / Page.page_size).
pagination_page(Page, next, PageNumber) :-
  \+ pagination_is_at_end(Page),
  PageNumber is Page.page_number + 1.
pagination_page(Page, prev, PageNumber) :-
  PageNumber is Page.page_number - 1,
  PageNumber > 0.



%! pagination_range(+Page:dict, -Range:pair(nonneg)) is det.
%
% Returns the range of results that is spanned by the given Page.

pagination_range(Page, 0-0) :-
  Page.number_of_results =:= 0, !.
pagination_range(Page, Low-High) :-
  Low is (Page.page_number - 1) * Page.page_size + 1,
  High is Low + Page.number_of_results - 1.
