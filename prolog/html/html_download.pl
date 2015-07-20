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
:- use_module(library(lambda)).
:- use_module(library(sgml)).





%! html_download(+Uri:atom, -Dom:compound) is det.

html_download(Uri, Dom):-
  http_get(
    Uri,
    \_^Read^load_html(Read, Dom, [dialect(html5),max_errors(-1)])
  ).
