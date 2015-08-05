:- module(html_resource, []).
:- reexport(library(http/html_head)).

/** <module> HTML resource

Initialize locations for serving HTML resources.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- dynamic(user:file_search_path/2).
:- multifile(user:file_search_path/2).

user:file_search_path(css, library(resource/css)).
user:file_search_path(img, library(resource/img)).
user:file_search_path(js, library(resource/js)).
user:file_search_path(ttl, library(resource/ttl)).

:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).
:- http_handler(ttl(.), serve_files_in_directory(ttl), [prefix]).
