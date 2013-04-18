run:-
  % Do not write module loads to the standard output stream.
  set_prolog_flag(verbose_load, silent),
  
  source_file(run, ThisFile),
  file_directory_name(ThisFile, ThisDirectory),
  assert(user:file_search_path(pgc, ThisDirectory)),
  assert(user:file_search_path(data, pgc('Data'))),
  
  % Load the PGC load file.
  ensure_loaded(pgc(load)),
  
  % Identity on the Web.
  ensure_loaded(pgc(identity)).

:- run.

