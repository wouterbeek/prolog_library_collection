:- module(
  rocks_ext,
  [
    call_on_rocks/2,   % +Alias, :Goal_1
    call_on_rocks/3,   % +Alias, :Goal_1, +Options
    rocks/3,           % +Db, ?Key, ?Value
    rocks_alias/2,     % ?Alias, -Directory
    rocks_clear/1,     % +Alias
    rocks_directory/1, % -Directory
    rocks_key/2,       % +DB, ?Key
    rocks_init/2,      % +Alias, +Options
    rocks_init/3,      % +Alias, -Db, +Options
    rocks_size/2       % +Db, -Size
  ]
).
:- reexport(library(rocksdb)).

/** <module> RocksDB extension

@author Wouter Beek
@version 2017/06-2017/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(conf_ext)).
:- use_module(library(debug)).
:- use_module(library(file_ext)).
:- use_module(library(option)).
:- use_module(library(rlimit)).
:- use_module(library(settings)).

:- meta_predicate
    call_on_rocks(+, 1),
    call_on_rocks(+, 1, +).

:- initialization
   init_rocks.

:- setting(rocks:directory, atom, .,
           "The directory where RocksDB instances are stored.").





%! call_on_rocks(+Alias:atom, :Goal_1) is det.
%! call_on_rocks(+Alias:atom, :Goal_1, +Options:list(compound)) is det.
%
% Calls Goal_1 on the RocksDB index called Alias.
%
% Options are passed to rocks_init/3.

call_on_rocks(Alias, Goal_1) :-
  call_on_rocks(Alias, Goal_1, []).


call_on_rocks(Alias, Goal_1, Options) :-
  setup_call_cleanup(
    rocks_init(Alias, Db, Options),
    call(Goal_1, Db),
    rocks_close(Db)
  ).



%! rocks(+Db, +Key:term, +Value:term) is semidet.
%! rocks(+Db, +Key:term, -Value:term) is semidet.
%! rocks(+Db, -Key:term, +Value:term) is nondet.
%! rocks(+Db, -Key:term, -Value:term) is nondet.
%
% Generalization of rocks_enum/3 and rocks_get/3.

rocks(Db, Key, Value) :-
  ground(Key), !,
  rocks_get(Db, Key, Value).
rocks(Db, Key, Value) :-
  rocks_enum(Db, Key, Value).



%! rocks_alias(+Alias:atom, -Db:blob) is det.
%! rocks_alias(-Alias:atom, -Db:blob) is nondet.
%
% Enumerates the exisiting RocksDB indices.

rocks_alias(Alias, Dir) :-
  rocks_directory(Dir0),
  directory_subdirectory(Dir0, Alias, Dir).



%! rocks_clear(+Alias:atom) is det.
%
% Assumes that the RocksDB index is already closed.

rocks_clear(Alias) :-
  rocks_directory(Dir0),
  directory_file_path(Dir0, Alias, Dir),
  (exists_directory(Dir) -> delete_directory_and_contents(Dir) ; true).



%! rocks_directory(-Directory:atom) is det.

rocks_directory(Dir) :-
  setting(rocks:directory, Dir0),
  % Allow rocks_directory to be a relative directory.
  absolute_file_name(Dir0, Dir, [access(write),file_type(directory)]).



%! rocks_key(+Db, +Key:term) is semidet.
%! rocks_key(+Db, -Key:term) is nondet.

rocks_key(Db, Key) :-
  rocks(Db, Key, _).



%! rocks_init(+Alias:atom, +Options:list(compound)) is det.
%! rocks_init(+Alias:atom, -Db, +Options:list(compound)) is det.
%
% Initializes a RocksDB database object.  This includes creating the
% directory structure, in case it is not already there.
%
% Options are passed to rocks_open/3.

rocks_init(Alias, Options1) :-
  merge_options([alias(Alias)], Options1, Options2),
  rocks_init(Alias, _, Options2).


rocks_init(Alias, Db, Options1) :-
  rocks_directory(Dir0),
  directory_file_path(Dir0, Alias, Dir),
  create_directory(Dir),
  merge_options([alias(Alias)], Options1, Options2),
  rocks_open(Dir, Db, Options2).



%! rocks_size(+Db, -Size:nonneg) is det.
%
% The Size of a RocksDB index is the number of unique pairs that are
% stored in it.

rocks_size(Db, Size) :-
  aggregate_all(
    count,
    rocks_enum(Db, _, _),
    Size
  ).





% MERGE OPERATORS %

%! rocks_merge_set(+Mode, +Key, +Left, +Right, -Result) is det.

rocks_merge_set(partial, _, Xs, Ys, Zs) :-
  ord_union(Xs, Ys, Zs),
  (debugging(rocks) -> debug_merge_set_([Xs,Ys]) ; true).
rocks_merge_set(full, _, Xs, Yss, Zs) :-
  ord_union([Xs|Yss], Zs),
  (debugging(rocks) -> debug_merge_set_([Xs|Yss]) ; true).

debug_merge_set_(Sets) :-
  maplist(length, Sets, Lengths),
  debug(rocks, "Set merge: ~p", [Lengths]).





% INITIALIZATION %

init_rocks :-
  conf_json(Conf0),
  (   _{rocks: Conf} :< Conf0
  ->  (   _{directory: Dir} :< Conf
      ->  create_directory(Dir),
          set_setting(rocks:directory, Dir)
      ;   true
      ),
      (   _{nofiles: NumFiles} :< Conf
      ->  rlimit(nofile, _, NumFiles)
      ;   true
      )
  ;   true
  ).
