:- module(
  json_ext,
  [
    json_open/2 % +File, -Dict
  ]
).
:- reexport(library(http/json)).

/** <module> JSON extensions

@author Wouter Beek
@version 2018
*/

:- use_module(library(yall)).

:- use_module(library(file_ext)).





%! json_open(+File:atom, -Dict:dict) is det.

json_open(File, Dict) :-
  call_stream_file(
    File,
    {Dict}/[In]>>json_read_dict(In, Dict, [value_string_as(atom)])
  ).
