:- module(
  nb_ext,
  [
    nb_increment/2, % +State, +Index
    nb_increment/3, % +State, +Index, -Value
    nb_plus/3,      % +State, +Index, +Increment
    nb_plus/4       % +State, +Index, +Increment, -Value
  ]
).

/** <module> Non-backtracking support

Extended support for non-backtracking behavior.

*/

:- use_module(library(clpfd)).



%! nb_increment(+State:compound, +Index:positive_integer) is det.

nb_increment(State, Index) :-
  nb_increment(State, Index, _).


%! nb_increment(+State:compound, +Index:positive_integer, -Value:nonneg) is det.

nb_increment(State, Index, Value) :-
  nb_plus(State, Index, 1, Value).



%! nb_plus(+State:compound, +Index:positive_integer, +Increment:number) is det.

nb_plus(State, Index, Increment) :-
  nb_plus(State, Index, Increment, _).


%! nb_plus(+State:compound,
%!         +Index:positive_integer,
%!         +Increment:number,
%!         -Value:nonneg) is det.

nb_plus(State, Index, Increment, Value1) :-
  arg(Index, State, Value1),
  Value2 #= Value1 + Increment,
  nb_setarg(Index, State, Value2).
