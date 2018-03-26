:- module(
  pair_ext,
  [
    sum_value/2 % +Pair1, -Pair2
  ]
).
:- reexport(library(pairs)).

/** <module> Pair extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(error)).
:- use_module(library(lists)).

:- multifile
    error:has_type/2.

error:has_type(pair(Type), Pair) :-
  error:has_type(pair(Type,Type), Pair).
error:has_type(pair(KeyType,ValueType), Key- Value) :-
  error:has_type(KeyType, Key),
  error:has_type(ValueType, Value).





%! sum_value(+Pair1:pair(term,number), -Pair2:pair(term,number)) is det.

sum_value(Key-Vals, Key-Val) :-
  sum_list(Vals, Val).
