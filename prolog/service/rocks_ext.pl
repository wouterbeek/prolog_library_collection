:- module(
  rocks_ext,
  [
    call_to_rocks/3,   % +Alias, +Type, :Goal_1
    rocks_merge_set/5, % +Mode, +Key, +Left, +Right, -Result
    rocks_merge_sum/5, % +Mode, +Key, +Left, +Right, -Result
    rocks_pull/3       % +Alias, -Key, -Val
  ]
).
:- reexport(library(rocksdb)).

/** <module> RocksDB extensions

@author Wouter Beek
@version 2016/08-2016/09, 2017/01
*/

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(settings)).

:- meta_predicate
    call_to_rocks(+, +, 1).

:- setting(
     index_dir,
     atom,
     '~/Data/index/',
     "Directory in whose subdirectories RocksDB indices are stored."
   ).





%! call_to_rocks(+Alias, +Type, :Goal_1) is det.

call_to_rocks(Alias, Type, Goal_1) :-
  setting(index_dir, Dir),
  directory_file_path(Dir, Alias, Subdir),
  once(type_merge_value(Type, Merge_5, Value)),
  setup_call_cleanup(
    rocks_open(
      Subdir,
      _,
      [alias(Alias),key(atom),merge(Merge_5),value(Value)]
    ),
    call(Goal_1, Alias),
    rocks_close(Alias)
  ).

type_merge_value(int, rocks_merge_sum, int64).
type_merge_value(set(atom), rocks_merge_set, int64).



%! rocks_merge_set(+Mode, +Key, +Left, +Right, -Result) is det.

rocks_merge_set(partial, _, X, Y, Z) :-
  ord_union(X, Y, Z).
rocks_merge_set(full, _, X, Y, Z) :-
  append([X|Y], XY),
  sort(XY, Z).



%! rocks_merge_sum(+Mode, +Key, +Left, +Right, -Result) is det.

rocks_merge_sum(partial, _, X, Y, Z) :-
  Z is X + Y.
rocks_merge_sum(full, _, Initial, Additions, Sum) :-
  sum_list([Initial|Additions], Sum).



%! rocks_pull(+Alias, -Key, -Val) is nondet.

rocks_pull(Alias, Key, Val) :-
  rocks_enum(Alias, Key, Val),
  rocks_delete(Alias, Key).
