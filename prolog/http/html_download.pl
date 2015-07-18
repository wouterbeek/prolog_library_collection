:- module(
  html_download,
  [
    html_download/2 % +Uri:atom
                    % -Dom:compound
  ]
).

/** <module> HTML download

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(http/http_request)).
:- use_module(library(sgml)).

:- predicate_options(load_html0/2, 2, [
     pass_to(load_html/3, 3)
   ]).





%! html_download(+Uri:atom, -Dom:compound) is det.

html_download(Uri, Dom):-
  http_get(Uri, load_html0(Dom, [dialect(html5),max_errors(-1)])).

load_html0(Dom, Opts, _, Read):-
  load_html(Read, Dom, Opts).
