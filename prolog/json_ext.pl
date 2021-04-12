:- module(
  json_ext,
  [
    json_load/2, % +File, -Structure
    json_save/2  % +File, +Structure
  ]
).
:- reexport(library(http/json)).

/** <module> Extended JSON support

*/

:- use_module(library(yall)).

:- use_module(library(file_ext)).





%! json_load(+File:atom, -Structure:dict) is det.

json_load(File, Struct) :-
  read_from_file(
    File,
    {Struct}/[In0]>>json_read_dict(In0, Struct, [value_string_as(atom)])
  ).



%! json_save(+File:atom, +Structure:dict) is det.

json_save(File, Struct) :-
  write_to_file(File, {Struct}/[Out0]>>json_write_dict(Out0, Struct)).
