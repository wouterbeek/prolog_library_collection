:- module(
  pagination,
  [
    create_pagination/3,    % +Pattern, :Goal_0, -Page
    create_pagination/4,    % +Pattern, :Goal_0, +Options, -Page
    create_pagination/5,    % +Pattern, :Goal_0, :Est_1, +Options, -Page
    pagination_is_at_end/1, % +Result
    pagination_is_empty/1,  % +Result
    pagination_page/3,      % +Page, ?Relation, -PageNumber
    pagination_range/2      % +Page, -Range
  ]
).

/** <module> Pagination

This module creates pages that group results.

```
?- create_pagination(N, between(1, 1000000, N), _{page: 856}, Result).
Result = _G120{number_of_results:20, page:856, page_size:20, results:[17101, 17102, 17103, 17104, 17105, 17106, 17107, 17108|...]} ;
Result = _G147{number_of_results:20, page:857, page_size:20, results:[17121, 17122, 17123, 17124, 17125, 17126, 17127, 17128|...]} ;
Result = _G147{number_of_results:20, page:858, page_size:20, results:[17141, 17142, 17143, 17144, 17145, 17146, 17147, 17148|...]} 
```

@author Wouter Beek
@version 2017/05-2017/08
*/

:- use_module(library(dict_ext)).
:- use_module(library(settings)).

:- meta_predicate
    create_pagination(+, 0, -),
    create_pagination(+, 0, +, -),
    create_pagination(+, 0, 1, +, -).


:- setting(
     default_page_size,
     positive_integer,
     20,
     "The default number of results per page."
   ).





%! create_empty_pagination(+Options:list(compound), -Page:dict) is det.
%
% Returns a pagination Page dictionary with empty results.

create_empty_pagination(Options, Page) :-
  create_pagination(_, fail, Options, Page).



%! create_pagination(+Templ, :Goal_0, -Page:dict) is nondet.
%! create_pagination(+Templ, :Goal_0, +Options:list(compound),
%!                   -Page:dict) is nondet.
%! create_pagination(+Templ, :Goal_0, :Est_1, +Options:list(compound),
%!                   -Page:dict) is nondet.
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
%     The number of results per (full) page.  The default is 100.
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
%   * total_number_of_results(nonneg)
%
%     The total number of results, independent of pagination.  This is
%     only present when etimation goal Est_1 is passed.

create_pagination(Templ, Goal_0, Page) :-
  create_pagination(Templ, Goal_0, _{}, Page).


create_pagination(Templ, Goal_0, Options1, Page2) :-
  pagination_options(Options1, StartPageNumber, PageSize, Options2),
  Counter = page(1),
  findnsols(PageSize, Templ, Goal_0, Results),
  arg(1, Counter, CurrentPageNumber1),
  CurrentPageNumber2 is CurrentPageNumber1 + 1,
  nb_setarg(1, Counter, CurrentPageNumber2),
  (   % Done if there are no results.
      Results == []
  ->  !, true
  ;   % Skip the current page if it appears before the start page.
      CurrentPageNumber1 < StartPageNumber
  ->  false
  ;   !, true
  ),
  length(Results, NumResults),
  Page1 = _{
    number_of_results: NumResults,
    page_number: CurrentPageNumber1,
    page_size: PageSize,
    results: Results
  },
  merge_dicts(Options2, Page1, Page2).
create_pagination(_, _, Options, Page) :-
  create_empty_pagination(Options, Page).


create_pagination(Templ, Goal_0, Est_1, Options1, Page2) :-
  create_pagination(Templ, Goal_0, Options1, Page1),
  call(Est_1, TotalNumResults),
  dict_put(total_number_of_results, Page1, TotalNumResults, Page2).



%! pagination_is_at_end(+Page:dict) is semidet.
%
% Succeeds if Page is the last page.
%
% Since we do not know the total number of results, the last page may
% be empty.

pagination_is_at_end(Page) :-
  Page.number_of_results < Page.page_size.



%! pagination_is_empty(+Page:dict) is semidet.

pagination_is_empty(Page) :-
  Page.number_of_results =:= 0.



%! pagination_options(+Options1:list(compound),
%!                    -StartPageNumber:positive_integer,
%!                    -PageSize:nonneg, -Options2:list(compound)) is det.

pagination_options(Options1, StartPageNumber, PageSize, Options3) :-
  dict_delete_or_default(page_number, Options1, 1, StartPageNumber, Options2),
  setting(default_page_size, DefaultPageSize),
  dict_delete_or_default(page_size, Options2, DefaultPageSize, PageSize, Options3).



%! pagination_page(+Page:dict, +Relation:atom,
%!                 -PageNumber:positive_integer) is semidet.
%! pagination_page(+Page:dict, -Relation:atom,
%                  -PageNumber:positive_integer) is nondet.
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
