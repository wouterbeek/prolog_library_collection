:- module(
  archive_ext,
  [
    archive_path/3 % +File, -Path, +Options
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2017/07, 2017/10
*/





%! archive_path(+File:atom, -Path:list(dict), +Options:list(compound)) is nondet.

archive_path(File, Path, Options) :-
  setup_call_cleanup(
    open(File, read, In, Options),
    setup_call_cleanup(
      archive_open(In, Archive, Options),
      setup_call_cleanup(
        archive_data_stream(Archive, In, [meta_data(Path)]),
        true,
        close(In)
      ),
      archive_close(Archive)
    ),
    close(In)
  ).
