:- module(
  pagination,
  [
    create_pagination_total/4 % :AllGoal_2, :SomeGoal_2, +PageOpts, -Result
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
    create_pagination_total(2, 2, +, -).





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
