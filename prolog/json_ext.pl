:- module(
  json_ext,
  [
    json_load/2, % +File, -Structure
    json_save/2  % +File, +Structure
  ]
).
:- reexport(library(http/json)).

/** <module> JSON extensions

@author Wouter Beek
@version 2018
*/

:- use_module(library(yall)).

:- use_module(library(file_ext)).





%! json_load(+File:atom, -Structure:dict) is det.

json_load(File, Structure) :-
  read_from_file(
    File,
    {Structure}/[In]>>json_read_dict(In, Structure, [value_string_as(atom)])
  ).



%! json_save(+File:atom, +Structure:dict) is det.

json_save(File, Structure) :-
  write_to_file(File, {Structure}/[Out]>>json_write_dict(Out, Structure)).
