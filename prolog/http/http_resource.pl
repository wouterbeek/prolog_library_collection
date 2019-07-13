:- module(http_resource, []).

/** <module> HTTP resource

Initialize locations for serving HTTP resources.

@author Wouter Beek
@version 2017-2019
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- use_module(library(resource)).

:- dynamic
    http:location/3.

:- multifile
    http:location/3.

http:location(css, root(css), []).
http:location(fonts, root(fonts), []).
http:location(html, root(html), []).
http:location(img, root(img), []).
http:location(js, root(js), []).
http:location(md, root(md), []).
http:location(pdf, root(pdf), []).
http:location(ttl, root(ttl), []).
http:location(yaml, root(yaml), []).

:- http_handler(css(.), serve_files_in_directory(css), [prefix]).
:- http_handler(fonts(.), serve_files_in_directory(fonts), [prefix]).
:- http_handler(html(.), serve_files_in_directory(html), [prefix]).
:- http_handler(img(.), serve_files_in_directory(img), [prefix]).
:- http_handler(js(.), serve_files_in_directory(js), [prefix]).
:- http_handler(md(.), serve_files_in_directory(md), [prefix]).
:- http_handler(pdf(.), serve_files_in_directory(pdf), [prefix]).
:- http_handler(ttl(.), serve_files_in_directory(ttl), [prefix]).
:- http_handler(yaml(.), serve_files_in_directory(yaml), [prefix]).
