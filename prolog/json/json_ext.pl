:- module(
  json_download,
  [
    json_download/2 % +Uri:atom
                    % -Json:dict
  ]
).

/** <module> JSON download

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(http/http_request)).
:- use_module(library(http/json)).





%! json_download(+Uri:atom, -Json:dict) is det.

json_download(Uri, Json):-
  http_get(Uri, json_read_dict0(Json)).

json_read_dict0(Json, _, Read):-
  json_read_dict(Read, Json).
