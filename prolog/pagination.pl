:- module(
  pagination,
  [
    pagination/4,       % +Pattern, :Goal_0, +Opts, -Result
    pagination_result/3 % +Query, +Result, :Goal_1
  ]
).

/** <module> Pagination

@author Wouter Beek
@verson 2016/03
*/

:- use_module(library(dict_ext)).
:- use_module(library(print_ext)).

:- meta_predicate
    pagination(+, 0, +, -),
    pagination_result(+, +, 1).





%! pagination(+Pattern, :Goal_0, +Opts, -Result) is nondet.
% The following options are supported:
%   - page_size(+nonneg)
%     Default is 100.
%   - page(+nonneg)
%     Default is 1.
%
% The following keys are in Result:
%   - number_of_results
%   - page
%   - page_size
%   - results

pagination(Pattern, Goal_0, Opts1, Result) :-
  mod_dict(page_size, Opts1, 100, PageSize, Opts2),
  mod_dict(page, Opts2, 1, StartPage, Opts3),
  put_dict(page0, Opts3, 0, Opts4),
  % NONDET
  findnsols(PageSize, Pattern, Goal_0, Results),
  length(Results, LenResults),
  dict_inc(page0, Opts4),
  (Opts4.page0 >= StartPage -> true ; false),
  Result = _{
    number_of_results: LenResults,
    page: Opts4.page0,
    page_size: PageSize,
    results: Results
  }.



%! pagination_result(+Query, +Result, :Goal_1) is det.

pagination_result(Query, Result, Goal_1) :-
  call(Goal_1, Result.results),
  nl,
  Opts = [fg(green)],
  (   Result.results == []
  ->  ansi_format(Opts, "No results for query '~a'.~n", [Query])
  ;   Low is (Result.page - 1) * Result.page_size,
      LowDisplay is Low + 1,
      High is Low + Result.number_of_results,
      ansi_format(
        Opts,
        "Showing results ~D -- ~D for query '~a'.~n",
        [LowDisplay,High,Query]
      )
  ).
