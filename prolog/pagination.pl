:- module(
  pagination,
  [
    pagination/3,        % +Pattern, :Goal_0, -Result
    pagination/4,        % +Pattern, :Goal_0, +Opts, -Result
    pagination_at_end/1, % +Result
    pagination_empty/2,  % +Opts, -Result
    pagination_first/2,  % +Result, -First
    pagination_next/2,   % +Result, -Next
    pagination_prev/2,   % +Result, -Prev
    pagination_range/2,  % +Result, -Range
    pagination_result/2, % +Result, :Goal_1
    pagination_total/4   % :AllGoal_2, :SomeGoal_2, +Opts, -Result
  ]
).

/** <module> Pagination

Generic support for pagination.

```prolog
?- pagination(N, between(1, 1000000, N), _{page: 856}, Results).
Results = _G120{number_of_results:20, page:856, page_size:20, results:[17101, 17102, 17103, 17104, 17105, 17106, 17107, 17108|...]} ;
Results = _G147{number_of_results:20, page:857, page_size:20, results:[17121, 17122, 17123, 17124, 17125, 17126, 17127, 17128|...]} ;
Results = _G147{number_of_results:20, page:858, page_size:20, results:[17141, 17142, 17143, 17144, 17145, 17146, 17147, 17148|...]} 
```

@author Wouter Beek
@verson 2016/03, 2016/05-2016/08
*/

:- use_module(library(dict_ext)).
:- use_module(library(print_ext)).
:- use_module(library(settings)).

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





%! pagination(+Pattern, :Goal_0, -Result) is nondet.
%! pagination(+Pattern, :Goal_0, +Opts, -Result) is nondet.
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
%   - page(positive_integer)
%
%   - page_size(positive_integer)
%
%   - results(nonneg)

pagination(Pattern, Goal_0, Result) :-
  pagination(Pattern, Goal_0, _{}, Result).


pagination(Pattern, Goal_0, Opts1, Result) :-
  page_size_options0(Opts1, PageSize, Opts2),
  findnsols(PageSize, Pattern, Goal_0, Results),
  pagination_result0(PageSize, Results, Result, Opts2).


pagination_result0(PageSize, Results, Result2, Opts1) :-
  length(Results, NumResults),
  mod_dict(page, Opts1, 1, StartPage, Opts2),
  put_dict(page0, Opts2, 0, Opts3),
  dict_inc(page0, Opts3),
  (   % No more results.
      NumResults =:= 0
  ->  true
  ;   % Skip pages that are before the start page.
      Opts3.page0 >= StartPage
  ->  true
  ;   false
  ),
  Result1 = _{
    number_of_results: NumResults,
    page: Opts3.page0,
    page_size: PageSize,
    results: Results
  },
  merge_dicts(Opts3, Result1, Result2).


page_size_options0(Opts1, PageSize, Opts2) :-
  setting(def_page_size, DefPageSize),
  mod_dict(page_size, Opts1, DefPageSize, PageSize, Opts2).



%! pagination_at_end(+Result) is semidet.
%
% Succeeds if pagination Results are at the last page.
%
% @note Since we do not know the total number of results, the last
%       page may be empty.

pagination_at_end(Result) :-
  Result.number_of_results < Result.page_size.



%! pagination_empty(+Opts, -Result) is det.
%
% Returns a pagination Result dictionary with empty results.

pagination_empty(Opts, Result) :-
  pagination(_, fail, Opts, Result).



%! pagination_first(+Result, -First) is semidet.
%
% Returns the first page for the given pagination Result.  Fails
% silently when there is not a single page.

pagination_first(Result, 1) :-
  Result.number_of_results > 0.



%! pagination_next(+Result, -Next) is semidet.
%
% Returns the next page for the given pagination Result.  Fails
% silently when there is no next page.

pagination_next(Result, Next) :-
  \+ pagination_at_end(Result),
  Next is Result.page + 1.



%! pagination_prev(+Result, -Prev) is semidet.
%
% Returns the next page for the given pagination Result.  Fails
% silently when there is no previous page.

pagination_prev(Result, Prev) :-
  Prev is Result.page - 1,
  Prev > 0.



%! pagination_range(+Result, -Range) is det.
%
% Returns the range spanned by the given pagination Result.

pagination_range(Result, 0-0) :-
  Result.number_of_results =:= 0, !.
pagination_range(Result, Low-High) :-
  Low is (Result.page - 1) * Result.page_size + 1,
  High is Low + Result.number_of_results - 1.



%! pagination_result(+Result, :Goal_1) is det.

pagination_result(Result, Goal_1) :-
  call(Goal_1, Result.results),
  nl,
  Opts = [fg(green)],
  (   Result.results == []
  ->  ansi_format(Opts, "No results for query.~n", [])
  ;   Low is (Result.page - 1) * Result.page_size,
      LowDisplay is Low + 1,
      High is Low + Result.number_of_results,
      ansi_format(
        Opts,
        "Showing results ~D -- ~D for query.~n",
        [LowDisplay,High]
      )
  ).



%! pagination_total(:AllGoal_2, :SomeGoal_2, +Opts, -Result) is nondet.
%
% The same options as pagination/4.
%
% The same keys as Result of pagination/4, plus:
%
%   - total_number_of_results(+nonneg)

pagination_total(AllGoal_2, SomeGoal_2, Opts1, Result2) :-
  page_size_options0(Opts1, PageSize, Opts2),
  call(AllGoal_2, AllResults, TotalNumResults),
  findnsols(
    PageSize,
    SomeResult,
    call(SomeGoal_2, AllResults, SomeResult),
    Results
  ),
  pagination_result0(PageSize, Results, Result1, Opts2),
  put_dict(total_number_of_results, Result1, TotalNumResults, Result2).
