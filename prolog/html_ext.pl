:- module(
  html_ext,
  [
    html_download/2, % +Uri, -Dom
    html_download/3  % +Uri, -Dom, +Options
  ]
).

/** <module> HTML support

Support for downloading HTML.

*/

:- use_module(library(sgml)).

:- use_module(library(http_client2)).





%! html_download(+Uri:atom, -Dom:compound) is semidet.
%! html_download(+Uri:atom, -Dom:compound, +Options:dict) is semidet.

html_download(Uri, Dom) :-
  html_download(Uri, Dom, options{}).


html_download(Uri, Dom, Options) :-
  http_open2(Uri, In, options{accept: html}),
  call_cleanup(
    load_html(In, Dom, Options),
    close(In)
  ).
