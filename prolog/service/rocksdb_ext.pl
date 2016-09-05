:- module(
  rocksdb_ext,
  [
    rocks_merge_sum/5 % +Mode, +Key, +Left, +Right, -Result
  ]
).

/** <module> RocksDB extensions

Merge operations for module rocksdb.

@author Wouter Beek
@version 2016/08-2016/09
*/

:- reexport(library(rocksdb)).





rocks_merge_sum(partial, _, X, Y, Z) :-
  Z is X + Y.
rocks_merge_sum(full, _, Initial, Additions, Sum) :-
  sum_list([Initial|Additions], Sum).
