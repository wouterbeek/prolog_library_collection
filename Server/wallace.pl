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

:- use_module(generics(db_ext)).
:- use_module(generics(logging)).
:- use_module(html(html)).
:- use_module(library(http/html_head)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_files)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_server_files)).
:- use_module(library(http/http_session)).
:- use_module(library(http/thread_httpd)).
:- use_module(server(error_web)).
:- use_module(server(web_console)).
:- use_module(sparql(sparql_web)).
:- use_module(standards(http)).

% This is used to push content for the console output and status pane.
:- dynamic(content_queue(_Type, _DTD_Name, _StyleName, _DOM)).

% By registering these modules, their Web predicates become accessible
% from the Web console.
:- register_module(web_message).

:- multifile(http:location/3).
:- dynamic(http:location/3).

% Serve CSS files.
http:location(css, root(css), []).
:- assert_novel(user:file_search_path(css, server(css))).
:- http_handler(css(.), serve_files_in_directory(css), [prefix]).

% Serve images.
%http:location(img, root(img), []).

% Serve JavaScript files.
http:location(js, root(js), []).
:- assert_novel(user:file_search_path(js, server(js))).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).

% Create a SPRAQL endpoint.
%http:location(sparql, root(sparql), []).
%:- http_handler(sparql(.), sparql, [spawn(sparql_query)]).

% HTTP handlers for the Wallace server.
:- http_handler(root(console_output), console_output, []).
:- http_handler(root(status_pane), status_pane, []).
:- http_handler(root(wallace), wallace, [prefix, priority(100)]).

% HTML resources and their dependencies.
:- html_resource(css('console_output.css'), [requires(css('wallace.css'))]).
:- html_resource(css('status_pane.css'), [requires(css('wallace.css'))]).
:- html_resource(js('console_output.js'), [requires(js('wallace.js'))]).
:- html_resource(js('status_pane.js'), [requires(js('wallace.js'))]).

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
  % =|web_message|= causes debug messages to be appended to the
  % current logging stream.
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
    \html_requires(css('console_output.css')),
    \html_requires(js('console_output.js'))
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
  wallace(Request).

push(Type, DOM):-
  push(Type, html, wallace, DOM).

push(Type, DTD_Name, StyleName, DOM):-
  %format(user, '[QQQ] ~w ~w ~w ~w\n', [Type, DTD_Name, StyleName, DOM]), %DEB
  assertz(content_queue(Type, DTD_Name, StyleName, DOM)).

status_pane(_Request):-
  retract(content_queue(status_pane, DTD_Name, Style_Name, DOM)),
  !,
  serve_xml(DTD_Name, Style_Name, DOM).
status_pane(Request):-
  serve_nothing(Request).

status_pane -->
  html([
    \html_requires(css('status_pane.css')),
    \html_requires(js('status_pane.js')),
    div(id(status_pane), [])
  ]).

test(Out):-
  format(Out, 'Content-type: application/xml~n~n', []),
  format(Out, 'Hello!', []).

user:body(wallace, _Body) -->
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

user:head(wallace, Head) -->
  {project_name(Project)},
  html(head(title(Project), Head)).

wallace(Request):-
  http_parameters(
    Request,
    [
      web_command(Command, [default(no_command)]),
      web_input(Query, [default(no_input)])
    ]
  ),
  (
    Command \== no_command
  ->
    web_console(Command, DTD_Name, StyleName, DOM),
    push(console_output, DTD_Name, StyleName, DOM)
  ;
    Query \== no_input
  ->
    sparql_output_web(Query, DOM),
    push(console_output, html, wallace, DOM)
  ;
    true
  ),
  serve_nothing(Request).
  %reply_html_page(wallace, [], []).

wallace_uri(URI):-
  default_port(Port),
  http_open:parts_uri(
    [host(localhost), path('/wallace/'), port(Port), scheme(http)],
    %[host('semanticweb.cs.vu.nl'), path('/prasem/'), scheme(http)],
    URI
  ).

