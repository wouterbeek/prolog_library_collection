:- module(
  closure,
  [
    path_closure/3, % :Goal_2, ?X, ?Y
    path_closure/4, % :Goal_2, ?X, ?Y, -Path
    path_distance/4 % :Goal_2, ?X, ?Y. -N
  ]
).

/** <module> Closure

@author Wouter Beek
@verion 2017/11
*/

:- use_module(library(error)).

:- meta_predicate
    path_closure(2, ?, ?),
    path_closure(2, ?, ?, -),
    path_closure1(2, +, -, +, -),
    path_closure2(2, -, +, +, -),
    path_distance(2, -, +, -),
    path_distance1(2, +, -, +, +, -),
    path_distance2(2, -, +, +, +, -).





%! path_closure(:Goal_2, -Y, +X) is nondet.
%! path_closure(:Goal_2, -Y, +X, -Path:list) is nondet.

path_closure(Goal_2, X, Y) :-
  path_closure(Goal_2, X, Y, _).


path_closure(Goal_2, X, Y, Path) :-
  ground(X), !,
  path_closure1(Goal_2, X, Y, [X], Path).
path_closure(Goal_2, X, Y, Path) :-
  ground(Y), !,
  path_closure2(Goal_2, X, Y, [X], Path).
path_closure(_, X, Y, _) :-
  instantiation_error(args(X,Y)).

path_closure1(_, X, X, Path, Path).
path_closure1(Goal_2, X, Z, Hist, Path) :-
  call(Goal_2, X, Y),
  \+ memberchk(Y, Hist),
  path_closure1(Goal_2, Y, Z, [Y|Hist], Path).

path_closure2(_, X, X, Path, Path).
path_closure2(Goal_2, Z, X, Hist, Path) :-
  call(Goal_2, Y, X),
  \+ memberchk(Y, Hist),
  path_closure2(Goal_2, Z, Y, [Y|Hist], Path).



%! path_distance(:Goal_2, +X, +Y, -N:nonneg) is semidet.
%! path_distance(:Goal_2, +X, -Y, -N:nonneg) is nondet.
%! path_distance(:Goal_2, -X, +Y, -N:nonneg) is nondet.

path_distance(Goal_2, X, Y, N) :-
  ground(X), !,
  path_distance1(Goal_2, X, Y, [X], 0, N).
path_distance(Goal_2, X, Y, N) :-
  ground(Y), !,
  path_distance2(Goal_2, X, Y, [X], 0, N).
path_distance(_, X, Y, _) :-
  instantiation_error(args(X,Y)).

path_distance1(_, X, X, _, N, N).
path_distance1(Goal_2, X, Z, Hist, N1, N) :-
  call(Goal_2, X, Y),
  \+ memberchk(Y, Hist),
  N2 is N1 + 1,
  path_distance1(Goal_2, Y, Z, [Y|Hist], N2, N).

path_distance2(_, X, X, _, N, N).
path_distance2(Goal_2, Z, X, Hist, N1, N) :-
  call(Goal_2, Y, X),
  \+ memberchk(Y, Hist),
  N2 is N1 + 1,
  path_distance2(Goal_2, Z, Y, [Y|Hist], N2, N).
