:- module(
  news,
  [
    run/1 % -Uri
  ]
).

:- use_module(library(http/http_download)).
:- use_module(library(xpath)).

run(Uri) :-
  html_download('http://lists.w3.org/Archives/Public/', Dom),
  xpath(Dom, //dt/a(1,@href), Uri).
