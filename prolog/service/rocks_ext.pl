:- module(
  rocks_ext,
  [
    rocks/3,        % +Db, ?Key, ?Val
    rocks_num/3,    % +Db, ?Key, ?Val
    rocks_put_num/3 % +Db, +Key, +Val
  ]
).

/** <module> RocksDB extensions

@author Wouter Beek
@version 2016/08
*/

:- reexport(library(rocksdb)).





%! rocks(+Db, ?Key, ?Val) is nondet.

rocks(Db, Key, Val) :-
  nonvar(Key), !,
  rocks_get(Db, Key, Val).
rocks(Db, Key, Val) :-
  rocks_enum(Db, Key, Val).



%! rocks_num(+Db, ?Key, ?Val) is nondet.

rocks_num(Db, Key, Val) :-
  rocks(Db, Key, Val0),
  atom_number(Val0, Val).



%! rocks_put_num(+Db, +Key, +Val) is nondet.

rocks_put_num(Db, Key, Y) :-
  (rocks(Db, Key, X0) -> atom_number(X0, X) ; X = 0),
  Z is X + Y,
  atom_number(Z0, Z),
  rocks_put(Db, Key, Z0).
