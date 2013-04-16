:- module(
  server,
  [
    start_server/1, % +Port:integer
    stop_server/1 % +Port:integer
  ]
).

/** <module> Server

@author Wouter Beek
@version Aug 2012
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/thread_httpd)).



start_server(Port):-
  %html_set_options([dialect(xhtml)]),
  http_server(http_dispatch, [port(Port)]).

stop_server(Port):-
  http_stop_server(Port, []).

