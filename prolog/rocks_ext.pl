:- module(
  rocks_ext,
  [
    call_on_rocks/2, % +Name, :Goal_1
    call_on_rocks/3, % +Name, :Goal_1, +Options
    rocks/3,         % +Db, ?Key, ?Value
    rocks_clear/1,   % +Name
    rocks_key/2,     % +DB, ?Key
    rocks_init/2,    % +Name, +Options
    rocks_init/3,    % +Name, -Db, +Options
    rocks_size/2     % +Db, -Size
  ]
).
:- reexport(library(rocksdb)).

/** <module> RocksDB extension

@author Wouter Beek
@version 2017/06-2017/10
*/

:- use_module(library(aggregate)).
:- use_module(library(conf_ext)).
:- use_module(library(file_ext)).
:- use_module(library(option)).
:- use_module(library(rlimit)).
:- use_module(library(settings)).

:- meta_predicate
    call_on_rocks(+, 1),
    call_on_rocks(+, 1, +).

:- rlimit(nofile, _, 8192).

:- setting(rocks_directory, atom, .,
           "The directory where RocksDB instances are stored.").

:- initialization
   conf_json(Dict),
   get_dict('rocksdb-directory', Dict, Dir),
   create_directory(Dir),
   set_setting(rocks_directory, Dir).





%! call_on_rocks(+Name:atom, :Goal_1) is det.
%! call_on_rocks(+Name:atom, :Goal_1, +Options:list(compound)) is det.
%
% Calls Goal_1 on the RocksDB index called Name.
%
% Options are passed to rocks_init/3.

call_on_rocks(Name, Goal_1) :-
  call_on_rocks(Name, Goal_1, []).


call_on_rocks(Name, Goal_1, Options) :-
  setup_call_cleanup(
    rocks_init(Name, Db, Options),
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



%! rocks_clear(+Name:atom) is det.

rocks_clear(Name) :-
  rocks_dir(Name, Dir),
  (exists_directory(Dir) -> delete_directory_and_contents(Dir) ; true).



%! rocks_key(+Db, +Key:term) is semidet.
%! rocks_key(+Db, -Key:term) is nondet.

rocks_key(Db, Key) :-
  rocks(Db, Key, _).



%! rocks_init(+Name:atom, +Options:list(compound)) is det,
%! rocks_init(+Name:atom, -Db, +Options:list(compound)) is det,
%
% Initializes a RocksDB database object.  This includes creating the
% directory structure, in case it is not already there.
%
% Options are passed to rocks_open/3.

rocks_init(Name, Options1) :-
  merge_options([alias(Name)], Options1, Options2),
  rocks_init(Name, _, Options2).


rocks_init(Name, Db, Options1) :-
  rocks_dir(Name, Dir),
  create_directory(Dir),
  merge_options([alias(Name)], Options1, Options2),
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





% HELPERS %

%! rocks_dir(+Name:atom, -Directory:atom) is det.

rocks_dir(Name, Dir3) :-
  setting(rocks_directory, Dir1),
  absolute_file_name(Dir1, Dir2, [access(write),file_type(directory)]),
  directory_file_path(Dir2, Name, Dir3).
