:- module(pair_ext, []).

/** <module> Pair extensions

@author Wouter Beek
@version 2017/07
*/

:- use_module(library(error)).

:- multifile
    error:has_type/2.

error:has_type(pair(Type), Pair) :-
  error:has_type(pair(Type,Type), Pair).
error:has_type(pair(KeyType,ValueType), Key- Value) :-
  error:has_type(KeyType, Key),
  error:has_type(ValueType, Value).
