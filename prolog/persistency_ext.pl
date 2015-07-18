:- module(
  persistency_ext,
  [
    attach_persistent_db/2, % +Module:atom
                            % +File:atom
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





%! attach_persistent_db(+Module:atom, +File:atom) is det.
% Safe attachement of a persistent database dump.
% This first make sure the given file exists.

attach_persistent_db(Mod, File):-
  exists_file(File), !,
  Mod:db_attach(File, []).
attach_persistent_db(Mod, File):-
  touch(File),
  attach_persistent_db(Mod, File).



%! init_persistent_db(+File:atom, :Goal) is det.
% Generic initialization of persistent databases.
%
% `Goal` is the update goal that is set by a specific persistent database.
% It takes the persistency file's age as an argument.

init_persistent_db(File, Module:Goal):-
  attach_persistent_db(Module, File),
  file_age(File, Age),
  call(Module:Goal, Age).
