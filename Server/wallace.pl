:- encoding(utf8).

:- module(
  wallace,
  [
    push/2, % +Type:oneof([console_output,status_pane])
            % +DOM:list
    push/4, % +Type:oneof([console_output,status_pane])
            % +DTD_Name:atom
            % +StyleName:atom
            % +DOM:list
    wallace_uri/1 % -URI:uri
  ]
).

/** <module> Wallace webserver

Using this module automatically starts the server.

http://semanticweb.cs.vu.nl/prasem/

@author Wouter Beek
@version 2012/05, 2012/09-2012/12, 2013/02-2013/04
*/

:- use_module(html(html)).
:- use_module(generics(logging)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/thread_httpd)).
:- use_module(server(error_web)).
:- use_module(server(web_console)).
:- use_module(sparql(sparql_web)).
:- use_module(standards(http)).

:- dynamic(content_queue(_Type, _DTD_Name, _StyleName, _DOM)).

% By registering these modules, their Web predicates become accessible
% from the Web console.
:- register_module(web_message).

:- multifile(http:location/3).
:- dynamic(http:location/3).

http:location(http_root,     '/prasem',        []).
http:location(http_db,       http_root('DB'),  []).
http:location(http_poems,    http_db('Poems'), []).
http:location(http_www,      http_root(www),   []).
http:location(http_www_css,  http_www(css),    []).
http:location(http_www_html, http_www(html),   []).
http:location(http_www_img,  http_www(img),    []).
http:location(http_www_js,   http_www(js),     []).
http:location(pldoc,         http_root(help),  [priority(1000)]).

:- http_handler(http_root(.), prasem, []).
:- http_handler(http_root(console_output), console_output, []).
:- http_handler(http_root(documentation), documentation, []).
:- http_handler(http_root(status_pane), status_pane, []).
:- http_handler(http_root(stcn), stcn, [id(stcn), prefix]).
:- http_handler(
  http_www(.),
  http_reply_from_files(prasem(www), []),
  [prefix]
).

:- html_resource(
  http_www_css('console_output.css'),
  [requires(http_www_css('prasem.css'))]
).
:- html_resource(
  http_www_js('console_output.js'),
  [requires(http_www_js('prasem.js'))]
).
:- html_resource(
  http_www_css('status_pane.css'),
  [requires(http_www_css('prasem.css'))]
).
:- html_resource(
  http_www_js('status_pane.js'),
  [requires(http_www_js('prasem.js'))]
).

:- multifile(user:prolog_file_type/2).

:- assert(user:prolog_file_type(htm,  html)).
:- assert(user:prolog_file_type(html, html)).

:- multifile(user:body//2).
:- multifile(user:head//2).



% START SERVER %

default_port(5000).

start_wallace:-
  default_port(Port),
  http_server_property(Port, start_time(_Time)),
  !.
start_wallace:-
  % Logging is required one Wallace is started, because module
  % =|web_message|= causes debug messages to be appended to the current
  % logging stream.
  start_log,
  
  default_port(Port),
  % Make sure Wallace is shut down whenever Prolog shuts down.
  assert(user:at_halt(http_stop_server(Port, []))),
  http_server(http_dispatch, [port(Port)]).
:- start_wallace.



% WEB FRONTEND %

console_output -->
  html([
    div(id(console_output), []),
    \html_requires(http_www_css('console_output.css')),
    \html_requires(http_www_js('console_output.js')),
    % This should be loaded on the fly.
    \html_requires(http_www_css('poem.css'))
  ]).

console_output(_Request):-
  retract(content_queue(console_output, DTD_Name, Style_Name, DOM)),
  !,
  serve_xml(DTD_Name, Style_Name, DOM).
console_output(Request):-
  serve_nothing(Request).

documentation(Request):-
  doc_browser,
  % Stay on the root page.
  prasem(Request).

prasem(Request):-
  http_parameters(Request, [
    web_command(Command, [default(no_command)]),
    web_input(Query, [default(no_input)])]),
  (
    Command \== no_command
  ->
    web_console(Command, DTD_Name, StyleName, DOM),
    push(console_output, DTD_Name, StyleName, DOM)
  ;
    Query \== no_input
  ->
    sparql_output_web(Query, DOM),
    push(console_output, html, prasem, DOM)
  ;
    true
  ),
  reply_html_page(prasem, [], []).

push(Type, DOM):-
  push(Type, html, prasem, DOM).

push(Type, DTD_Name, StyleName, DOM):-
  assertz(content_queue(Type, DTD_Name, StyleName, DOM)).

status_pane(_Request):-
  retract(content_queue(status_pane, DTD_Name, Style_Name, DOM)),
  !,
  serve_xml(DTD_Name, Style_Name, DOM).
status_pane(Request):-
  serve_nothing(Request).

status_pane -->
  html([
    \html_requires(http_www_css('status_pane.css')),
    \html_requires(http_www_js('status_pane.js')),
    div(id(status_pane), [])
  ]).

test(Out):-
  format(Out, 'Content-type: application/xml~n~n', []),
  format(Out, 'Hello!', []).

user:body(prasem, _Body) -->
  html(
    body(
      onload('loadConsoleOutputFunctions(); loadStatusPaneFunctions();'),
      [
        \console_input,
        \console_output,
        \status_pane
      ]
    )
  ).

user:body(stcn, Body) -->
  html(body(Body)).

user:head(prasem, Head) -->
  html(head(title('PraSem - Pragmatic Semantics for the Web of Data'), Head)).

user:head(stcn, Head) -->
  html(head([\html_requires(http_www_css('prasem.css')) | Head])).

wallace_uri(URI):-
  http_open:parts_uri(
    [host(localhost), path('/prasem/'), port(5000), scheme(http)],
    %[host('semanticweb.cs.vu.nl'), path('/prasem/'), scheme(http)],
    URI
  ).

