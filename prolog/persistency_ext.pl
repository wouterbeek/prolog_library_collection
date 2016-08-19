:- module(
  persistency_ext,
  [
    init_db/2 % +File, :Goal_0
  ]
).

/** <module> Persistency extensions

@author Wouter Beek
@version 2016/08
*/

:- use_module(library(os/file_ext)).
:- use_module(library(os/gnu_sort)).
:- reexport(library(persistency)).

:- meta_predicate
    init_db(+, 0).





%! init_db(+File, :Goal_0) is det.

init_db(File, _) :-
  access_file(File, read), !,
  db_attach(File, [sync(flush)]).
init_db(File, Goal_0) :-
  touch(File),
  db_attach(File, [sync(flush)]),
  call(Goal_0),
  sort_file(File).
