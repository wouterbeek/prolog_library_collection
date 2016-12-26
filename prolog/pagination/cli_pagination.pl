:- module(
  cli_pagination,
  [
    cli_pagination_result/2,         % +Result, :Goal_1
    cli_pagination_result_nonempty/2 % +Result, :Goal_1
  ]
).
:- reexport(library(pagination/pagination)).

/** <module> CLI pagination

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(ansi_term)).

:- meta_predicate
    cli_pagination_result(+, 1),
    cli_pagination_result_nonempty(+, 1).





%! cli_pagination_result(+Result, :Goal_1) is det.

cli_pagination_result(Result, Goal_1) :-
  % @note We give the totality of results at once, because a
  % collection of results may be displayed differently than a sequence
  % of independent results.
  call(Goal_1, Result.results),
  nl,
  Attrs = [fg(green)],
  (   Result.results == []
  ->  ansi_format(Attrs, "No results for query.~n", [])
  ;   Low is (Result.page - 1) * Result.page_size,
      LowDisplay is Low + 1,
      High is Low + Result.number_of_results,
      ansi_format(
        Attrs,
        "Showing results ~D -- ~D for query.~n",
        [LowDisplay,High]
      )
  ).



%! cli_pagination_result_nonempty(+Result, :Goal_1) is det.

cli_pagination_result_nonempty(Result, _) :-
  pagination_is_empty(Result), !.
cli_pagination_result_nonempty(Result, Goal_1) :-
  cli_pagination_result(Result, Goal_1).
