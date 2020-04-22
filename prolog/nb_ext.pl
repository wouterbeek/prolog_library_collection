:- module(
  nb_ext,
  [
    nb_increment/2, % +State, +Index
    nb_plus/3       % +State, +Index, +Value
  ]
).

/** <module> Non-backtracking support

Extended support for non-backtracking behavior.

*/





%! nb_increment(+State:compound, +Index:positive_integer) is det.

nb_increment(State, Index) :-
  nb_plus(State, Index, 1).



%! nb_plus(+State:compound, +Index:positive_integer, +Value:number) is det.

nb_plus(State, Index, Value) :-
  arg(Index, State, Value1),
  Value2 is Value1 + Value,
  nb_setarg(Index, State, Value2).
