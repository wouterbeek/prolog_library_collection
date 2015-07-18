:- use_module(library(ansi_term)).
:- use_module(library(prolog_pack)).

pack_ensure_removed(Pack):-
  pack_property(Pack, _), !,
  pack_remove(Pack).
pack_ensure_removed(_).

test:-
  pack_ensure_removed(plc),
  source_file(test, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(plcTest, ThisDirectory)),
  assert(user:file_search_path(library, plcTest('../prolog'))).
:- test.
