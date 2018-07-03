:- module(
  json_ext,
  [
    json_load/2, % +File, -Dict
    json_save/2  % +File, +Dict
  ]
).
:- reexport(library(http/json)).

/** <module> JSON extensions

@author Wouter Beek
@version 2018
*/

:- use_module(library(yall)).

:- use_module(library(file_ext)).





%! json_load(+File:atom, -Dict:dict) is det.

json_load(File, Dict) :-
  call_stream_file(
    File,
    {Dict}/[In]>>json_read_dict(In, Dict, [value_string_as(atom)])
  ).



%! json_save(+File:atom, +Dict:dict) is det.

json_save(File, Dict) :-
  call_stream_file(File, write, {Dict}/[Out]>>json_write_dict(Out, Dict)).
