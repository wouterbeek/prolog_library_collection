:- module(
  http_pagination,
  [
    http_pagination_header/1, % +Result
    http_pagination_uri/3,    % +Result, ?Rel, -Uri
    http_pagination_uris/2    % +Result, -Pairs
  ]
).
:- reexport(library(pagination/pagination_server)).

/** <module> HTTP pagination

@author Wouter Beek
@version 2016/12
*/

:- use_module(library(dict_ext)).
:- use_module(library(uri)).





%! http_pagination_header(+Result) is semidet.

http_pagination_header(Result) :-
  http_pagination_uris(Result, Pairs),
  format("Link: "),
  http_pagination_header0(Pairs),
  nl.

http_pagination_header0([]) :- !.
http_pagination_header0([H]) :- !,
  http_pagination_header_comp0(H).
http_pagination_header0([H|T]) :- !,
  http_pagination_header_comp0(H),
  format(", "),
  http_pagination_header0(T).

http_pagination_header_comp0(Rel-Uri) :-
  format('<~a>; rel="~a"', [Uri,Rel]).
