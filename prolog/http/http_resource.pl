:- module(http_resource, []).
:- reexport(library(http/html_head)).

/** <module> HTTP resource

Initialize locations for serving HTTP resources.

@author Wouter Beek
@version 2015/08-2017/03
*/

:- use_module(library(http/http_cors)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- dynamic
    http:location/3,
    user:file_search_path/2,
    user:prolog_file_type/2.

:- multifile
    http:location/3,
    user:file_search_path/2,
    user:prolog_file_type/2.

http:location(css, root(css), []).
http:location(fonts, root(fonts), []).
http:location(html, root(html), []).
http:location(img, root(img), []).
http:location(js, root(js), []).
http:location(md, root(md), []).
http:location(mp3, root(mp3), []).
http:location(pdf, root(pdf), []).
http:location(rdf, root(rdf), []).

user:prolog_file_type(htm,  html).
user:prolog_file_type(html, html).

user:file_search_path(resource, library(resource)).
  user:file_search_path(css, resource(css)).
  user:file_search_path(fonts, resource(fonts)).
  user:file_search_path(html, resource(html)).
  user:file_search_path(img, resource(img)).
  user:file_search_path(js, resource(js)).
  user:file_search_path(md, resource(md)).
  user:file_search_path(mp3, resource(mp3)).
  user:file_search_path(pdf, resource(pdf)).
  user:file_search_path(rdf, resource(rdf)).

:- http_handler(css(.), serve_files_in_directory_cors(css), [prefix]).
:- http_handler(fonts(.), serve_files_in_directory_cors(fonts), [prefix]).
:- http_handler(img(.), serve_files_in_directory_cors(img), [prefix]).
:- http_handler(js(.), serve_files_in_directory_cors(js), [prefix]).
:- http_handler(md(.), serve_files_in_directory_cors(md), [prefix]).
:- http_handler(mp3(.), serve_files_in_directory_cors(mp3), [prefix]).
:- http_handler(pdf(.), serve_files_in_directory_cors(pdf), [prefix]).
:- http_handler(rdf(.), serve_files_in_directory_cors(rdf), [prefix]).

serve_files_in_directory_cors(Alias, Req) :-
  cors_enable(Req, []),
  serve_files_in_directory(Alias, Req).
