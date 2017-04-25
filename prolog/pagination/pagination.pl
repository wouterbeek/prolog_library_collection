:- module(
  pagination,
  [
    create_empty_pagination/2, % +PageOpts, -Result
    create_pagination/3,       % +Pattern, :Goal_0, -Result
    create_pagination/4,       % +Pattern, :Goal_0, +PageOpts, -Result
    create_pagination_total/4, % :AllGoal_2, :SomeGoal_2, +PageOpts, -Result
    pagination_is_at_end/1,    % +Result
    pagination_is_empty/1,     % +Result
    pagination_page/3,         % +Result, ?Rel, -Page
    pagination_range/2         % +Result, -Range
  ]
).

/** <module> Pagination

Generic support for pagination.

```prolog
?- create_pagination(N, between(1, 1000000, N), _{page: 856}, Result).
Result = _G120{number_of_results:20, page:856, page_size:20, results:[17101, 17102, 17103, 17104, 17105, 17106, 17107, 17108|...]} ;
Result = _G147{number_of_results:20, page:857, page_size:20, results:[17121, 17122, 17123, 17124, 17125, 17126, 17127, 17128|...]} ;
Result = _G147{number_of_results:20, page:858, page_size:20, results:[17141, 17142, 17143, 17144, 17145, 17146, 17147, 17148|...]} 
```

@author Wouter Beek
@verson 2016/03-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).
:- use_module(library(lists)).
:- use_module(library(settings)).

:- meta_predicate
    create_pagination(+, 0, -),
    create_pagination(+, 0, +, -),
    create_pagination_total(2, 2, +, -).

:- setting(
     default_page_size,
     positive_integer,
     20,
     "The default number of results per page."
   ).





%! create_empty_pagination(+PageOpts, -Result) is det.
%
% Returns a pagination Result dictionary with empty results.

create_empty_pagination(PageOpts, Result) :-
  create_pagination(_, fail, PageOpts, Result).



%! create_pagination(+Pattern, :Goal_0, -Result) is nondet.
%! create_pagination(+Pattern, :Goal_0, +PageOpts, -Result) is nondet.
%
% The following options are supported:
%
%   - page_size(+nonneg) Default is 100.
%
%   - page(+nonneg) Default is 1.
%
% The following keys are in Result:
%
%   - number_of_results(nonneg)
%
%     The number of results on one page.  This is either identical to
%     or less than the value of ‘page_size’.
%
%   - page(positive_integer)
%
%   - page_size(positive_integer)
%
%   - results(nonneg)

create_pagination(Pattern, Goal_0, Result) :-
  create_pagination(Pattern, Goal_0, _{}, Result).


create_pagination(Pattern, Goal_0, PageOpts1, Result2) :-
  pagination_init_options(PageOpts1, StartPage, PageSize, PageOpts2),
  put_dict(page0, PageOpts2, 0, PageOpts3),
  findnsols(PageSize, Pattern, Goal_0, Results),
  dict_inc(page0, PageOpts3),
  length(Results, NumResults),
  (   % No more results.
      NumResults < PageSize
  ->  !, true
  ;   % Skip pages that are before the start page.
      PageOpts3.page0 >= StartPage
  ->  true
  ;   false
  ),
  Result1 = _{
    number_of_results: NumResults,
    page: PageOpts3.page0,
    page_size: PageSize,
    results: Results
  },
  merge_dicts(PageOpts3, Result1, Result2).
create_pagination(_, _, PageOpts, Result) :-
  create_empty_pagination(PageOpts, Result).



%! create_pagination_total(
%!   :AllGoal_2,
%!   :SomeGoal_2,
%!   +PageOpts,
%!   -Result
%! ) is nondet.
%
% The same options as pagination/4.
%
% The same keys as Result of pagination/4, plus:
%
%   - total_number_of_results(+nonneg)

create_pagination_total(AllGoal_2, SomeGoal_2, PageOpts1, Result2) :-
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
  Result1 = _{
    number_of_results: NumResults,
    page: PageOpts3.page0,
    page_size: PageSize,
    results: Results,
    total_number_of_results: TotalNumResults
  },
  merge_dicts(PageOpts3, Result1, Result2).



%! pagination_is_at_end(+Result) is semidet.
%
% Succeeds if pagination Results are at the last page.
%
% @note Since we do not know the total number of results, the last
%       page may be empty.

pagination_is_at_end(Result) :-
  Result.number_of_results < Result.page_size.



%! pagination_is_empty(+Result) is semidet.

pagination_is_empty(Result) :-
  Result.number_of_results =:= 0.



%! pagination_init_options(
%!   +PageOpts1,
%!   -StartPage,
%!   -PageSize,
%!   -PageOpts2
%! ) is det.

pagination_init_options(PageOpts1, StartPage, PageSize, PageOpts3) :-
  del_dict_or_default(page, PageOpts1, 1, StartPage, PageOpts2),
  setting(default_page_size, DefPageSize),
  del_dict_or_default(page_size, PageOpts2, DefPageSize, PageSize, PageOpts3).



%! pagination_page(+Result, +Rel, -Page) is semidet.
%! pagination_page(+Result, -Rel, -Page) is nondet.
%
% Page is the first, last, next, and previous page number for Result.
%
% Fails silently when there is no page with relation Rel.
%
% @arg Rel is either ‘first’, ‘last’, ‘next’, or ‘prev’.
% @arg Page is a positive integer.

pagination_page(Result, first, 1) :-
  Result.number_of_results > 0.
pagination_page(Result, last, Page) :-
  get_dict(total_number_of_results, Result, TotalNumResults),
  Page is ceil(TotalNumResults / Result.page_size).
pagination_page(Result, next, Page) :-
  \+ pagination_is_at_end(Result),
  Page is Result.page + 1.
pagination_page(Result, prev, Page) :-
  Page is Result.page - 1,
  Page > 0.



%! pagination_range(+Result, -Range) is det.
%
% Returns the range spanned by the given pagination Result.

pagination_range(Result, 0-0) :-
  Result.number_of_results =:= 0, !.
pagination_range(Result, Low-High) :-
  Low is (Result.page - 1) * Result.page_size + 1,
  High is Low + Result.number_of_results - 1.
