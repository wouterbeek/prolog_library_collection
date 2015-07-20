:- module(
  persistency_ext,
  [
    init_persistent_db/2 % +File:atom
                         % :Goal_1
  ]
).
:- reexport(library(persistency)).

/** <module> Persistency extensions

Extensions to functionality in library(persistency).

@author Wotuer Beek
@version 2015/07
*/

:- use_module(library(file_ext)).
:- use_module(library(persistency)).

:- meta_predicate(init_persistent_db(+,1)).





%! init_persistent_db(+File:atom, :Goal_1) is det.
% Generic initialization of persistent databases.
%
% `Goal` is the update goal that is set by a specific persistent database.
% It takes the persistency file's age as an argument.

init_persistent_db(File, Mod:Goal_1):-
  (   exists_file(File)
  ->  true
  ;   touch(File)
  ),
  db_attach(File, []),
  file_age(File, Age),
  call(Mod:Goal_1, Age).
