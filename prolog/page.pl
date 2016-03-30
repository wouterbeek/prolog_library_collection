:- module(
  page,
  [
    page/4 % +Pattern, :Goal_0, +Opts, -Result
  ]
).

/** <module> Pagination

@author Wouter Beek
@verson 2016/03
*/

:- use_module(library(dict_ext)).

:- meta_predicate
    page(+, 0, +, -).





%! page(+Pattern, :Goal_0, +Opts, -Result) is nondet.
% The following options are supported:
%   - page_size(+nonneg)
%     Default is 100.
%   - start_page(+nonneg)
%     Default is 1.
%
% The following keys are in Result:
%   - number_of_results
%   - page
%   - results

page(Pattern, Goal_0, Opts, Result) :-
  mod_dict(page_size, Opts1, 100, PageSize, Opts2),
  mod_dict(start_page, Opts2, 1, StartPage, Opts3),
  put_dict(page0, Opts3, 0, Opts4),
  % NONDET
  findnsols(PageSize, Pattern, Goal_0, Results),
  length(Results, LenResults),
  dict_inc(page0, Opts4),
  (Opts4.page0 >= StartPage -> true ; false),
  Result = _{
    number_of_results: LenResults,
    page: Opts4.page0,
    results: Results
  }.
