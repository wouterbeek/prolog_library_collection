:- module(
  pagination,
  [
    http_pagination_links/1, % +Result
    pagination/3,            % +Pattern, :Goal_0, -Result
    pagination/4,            % +Pattern, :Goal_0, +Opts, -Result
    pagination_at_end/1,     % +Result
    pagination_empty/2,      % +Opts, -Result
    pagination_iri/3,        % +Result, ?Rel, -Iri
    pagination_page/3,       % +Result, ?Rel, -Page
    pagination_range/2,      % +Result, -Range
    pagination_result/2,     % +Result, :Goal_1
    pagination_total/4       % :AllGoal_2, :SomeGoal_2, +Opts, -Result
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





%! http_pagination_links(+Result) is semidet.

http_pagination_links(Result) :-
  pagination_iris(Result, Rels, Iris),
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


pagination(Pattern, Goal_0, Opts1, Result2) :-
  setting(def_page_size, DefPageSize),
  mod_dict(page_size, Opts1, DefPageSize, PageSize, Opts2),
  mod_dict(page, Opts2, 1, StartPage, Opts3),
  put_dict(page0, Opts3, 0, Opts4),
  findnsols(PageSize, Pattern, Goal_0, Results),
  dict_inc(page0, Opts4),
  length(Results, NumResults),
  (   % No more results.
      NumResults =:= 0
  ->  true
  ;   % Skip pages that are before the start page.
      Opts4.page0 >= StartPage
  ->  true
  ;   false
  ),
  Result1 = _{
    number_of_results: NumResults,
    page: Opts4.page0,
    page_size: PageSize,
    results: Results
  },
  merge_dicts(Opts4, Result1, Result2).



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



%! pagination_iri(+Result, +Rel, -Iri) is semidet.
%! pagination_iri(+Result, -Rel, -Iri) is nondet.

pagination_iri(Result, Rel, Iri) :-
  pagination_page(Result, Rel, Page),
  get_dict(query, Result, QueryComps, []),
  uri_query_components(Query, [page(Page)|QueryComps]),
  uri_components(Result.iri, uri_components(Scheme,Auth,Path,_,_)),
  uri_components(Iri, uri_components(Scheme,Auth,Path,Query,_)).



%! pagination_iris(+Result, -Rels, -Iris) is det.
%
% Returns the Page for the given pagination Result.  Fails silently
% when there are no pages with relation Rel.

pagination_iris(Result, Rels, Iris) :-
  findall(Rel-Iri, pagination_iri(Result, Rel, Iri), Pairs),
  pairs_keys_values(Pairs, Rels, Iris).



%! pagination_page(+Result, +Rel, -Iri) is semidet.
%! pagination_page(+Result, -Rel, -Iri) is nondet.

pagination_page(Result, first, 1) :-
  Result.number_of_results > 0.
pagination_page(Result, last, Page) :-
  get_dict(total_number_of_results, Result, TotalNumResults),
  Page is ceil(TotalNumResults / Result.page_size).
pagination_page(Result, next, Page) :-
  \+ pagination_at_end(Result),
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
  setting(def_page_size, DefPageSize),
  mod_dict(page_size, Opts1, DefPageSize, PageSize, Opts2),
  mod_dict(page, Opts2, 1, StartPage, Opts3),
  put_dict(page0, Opts3, 0, Opts4),
  call(AllGoal_2, AllResults, TotalNumResults),
  findnsols(
    PageSize,
    SomeResult,
    call(SomeGoal_2, AllResults, SomeResult),
    Results
  ),
  dict_inc(page0, Opts4),
  length(Results, NumResults),
  (   % No more results.
      NumResults =:= 0
  ->  true
  ;   % Skip pages that are before the start page.
      Opts4.page0 >= StartPage
  ->  true
  ;   false
  ),
  Result1 = _{
    number_of_results: NumResults,
    page: Opts4.page0,
    page_size: PageSize,
    results: Results,
    total_number_of_results: TotalNumResults
  },
  merge_dicts(Opts4, Result1, Result2).
