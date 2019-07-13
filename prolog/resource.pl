:- module(resource, []).

/** <module> Resource

Sets common resource directories.

@author Wouter Beek
@version 2017-2019
*/

:- dynamic
    user:file_search_path/2.

:- multifile
    user:file_search_path/2.

% parent
user:file_search_path(resource, library(resource)).

% children
user:file_search_path(css, resource(css)).
user:file_search_path(data, resource(data)).
user:file_search_path(fonts, resource(fonts)).
user:file_search_path(html, resource(html)).
user:file_search_path(img, resource(img)).
user:file_search_path(js, resource(js)).
user:file_search_path(md, resource(md)).
user:file_search_path(pdf, resource(pdf)).
user:file_search_path(ttl, resource(ttl)).
user:file_search_path(yaml, resource(yaml)).
