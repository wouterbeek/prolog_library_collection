:- module(
  html_pagination,
  [
    html_pagination_links//1,          % +Result
    html_pagination_result//2,         % +Result, :Html_1
    html_pagination_result_nonempty//2 % +Result, :Html_1
  ]
).
:- reexport(library(pagination/http_pagination)).

/** <module> HTML pagination

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(html/html_ext)).
:- use_module(library(http/html_write)).

:- meta_predicate
    html_pagination_result(+, 3, ?, ?),
    html_pagination_result_nonempty(+, 3, ?, ?).





%! html_pagination_links(+Result)// is det.

html_pagination_links(Result) -->
  {http_pagination_uris(Result, Pairs)},
  html_maplist(link, Pairs).



%! html_pagination_pager(+Result)// is det.

html_pagination_pager(Result) -->
  html(
    nav(
      ul([
        \html_pagination_pager_prev(Result),
        " ",
        \html_pagination_range(Result),
        " ",
        \html_pagination_pager_next(Result)
      ])
    )
  ).



%! html_pagination_pager_prev(+Result)// is det.

html_pagination_pager_prev(Result) -->
  {
    (   http_pagination_uri(Result, prev, Prev)
    ->  AAttrs = [href=Prev],
        LiAttrs = []
    ;   AAttrs = [],
        LiAttrs = [class=disabled]
    )
  },
  html(li(LiAttrs, a(AAttrs, "Previous"))).



%! html_pagination_pager_next(+Result)// is det.

html_pagination_pager_next(Result) -->
  {
    (   http_pagination_uri(Result, next, Next)
    ->  AAttrs = [href=Next],
        LiAttrs = []
    ;   AAttrs = [],
        LiAttrs = [class=disabled]
    )
  },
  html(li(LiAttrs, a(AAttrs, "Next"))).



%! html_pagination_range(+Result)// is det.

html_pagination_range(Result) -->
  {pagination_range(Result, Low-High)},
  html("~:D ⎯⎯⎯ ~:D"-[Low,High]).



%! html_pagination_result(+Result, :Html_1)// is det.
%
% Opts are required because it contains the `uri` based on which the
% backward/forward request URIs are build.
%
% Result contains the following keys: ‘number_of_results’,
% ‘page’, ‘page_size’.
%
% Result must contain the keys ‘uri’ and ‘query’.

html_pagination_result(Result, Html_1) -->
  html_call(Html_1, Result.results),
  html_pagination_pager(Result).



%! html_pagination_result_nonempty(+Result, :Html_1)// is det.
%
% Like html_pagination_result/2, but does not generate anything if
% Result is empty.

html_pagination_result_nonempty(Result, _) -->
  {pagination_is_empty(Result)}, !, [].
html_pagination_result_nonempty(Result, Html_1) -->
  html_pagination_result(Result, Html_1).
