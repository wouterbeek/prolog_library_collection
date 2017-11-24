:- module(
  closure,
  [
    leave_closure/3,  % :Goal_2, ?X, ?Y
    leave_closure0/3, % :Goal_2, ?X, ?Y
    path_closure/3,   % :Goal_2, ?X, ?Y
    path_closure/4,   % :Goal_2, ?X, ?Y, -Path
    path_closure0/3,  % :Goal_2, ?X, ?Y
    path_closure0/4,  % :Goal_2, ?X, ?Y, -Path
    path_distance/4   % :Goal_2, ?X, ?Y. -N
  ]
).

/** <module> Closure

@author Wouter Beek
@verion 2017/11
*/

:- use_module(library(error)).

:- meta_predicate
    leave_closure(2, ?, ?),
    leave_closure0(2, ?, ?),
    path_closure(2, ?, ?),
    path_closure(2, ?, ?, -),
    path_closure0(2, ?, ?),
    path_closure0(2, ?, ?, -),
    path_closure_1(2, +, -, +, -),
    path_closure_2(2, -, +, +, -),
    path_distance(2, -, +, -),
    path_distance1(2, +, -, +, +, -),
    path_distance2(2, -, +, +, +, -).





%! leave_closure(:Goal_2, +X, +Y) is semidet.
%! leave_closure(:Goal_2, +X, -Y) is nondet.
%! leave_closure(:Goal_2, -X, +Y) is nondet.

leave_closure(Goal_2, X, Z) :-
  ground(X), !,
  call(Goal_2, X, Y),
  leave_closure_1(Goal_2, Y, Z, [X,Y]).
leave_closure(Goal_2, X, Z) :-
  ground(Z), !,
  call(Goal_2, Y, Z),
  leave_closure_2(Goal_2, X, Y, [Y,Z]).
leave_closure(_, X, Y) :-
  instantiation_error(args(X,Y)).



%! leave_closure0(:Goal_2, +X, +Y) is semidet.
%! leave_closure0(:Goal_2, +X, -Y) is nondet.
%! leave_closure0(:Goal_2, -X, +Y) is nondet.

leave_closure0(Goal_2, X, Y) :-
  ground(X), !,
  leave_closure_1(Goal_2, X, Y, [X]).
leave_closure0(Goal_2, X, Y) :-
  ground(Y), !,
  leave_closure_2(Goal_2, X, Y, [Y]).
leave_closure0(_, X, Y) :-
  instantiation_error(args(X,Y)).

leave_closure_1(Goal_2, X, Z, Hist) :-
  call(Goal_2, X, Y),
  \+ memberchk(Y, Hist), !,
  leave_closure_1(Goal_2, Y, Z, [Y|Hist]).
leave_closure_1(_, X, X, _).

leave_closure_2(Goal_2, X, Z, Hist) :-
  call(Goal_2, Y, Z),
  \+ memberchk(Y, Hist), !,
  leave_closure_2(Goal_2, X, Y, [Y|Hist]).
leave_closure_2(_, X, X, _).



%! path_closure(:Goal_2, +X, +Y) is semidet.
%! path_closure(:Goal_2, +X, -Y) is nondet.
%! path_closure(:Goal_2, -X, +Y) is nondet.
%! path_closure(:Goal_2, +X, +Y, -Path:list) is semidet.
%! path_closure(:Goal_2, +X, -Y, -Path:list) is nondet.
%! path_closure(:Goal_2, -X, +Y, -Path:list) is nondet.
%
% Transitive closure over X or Y.

path_closure(Goal_2, X, Y) :-
  path_closure(Goal_2, X, Y, _).


path_closure(Goal_2, X, Z, Path) :-
  ground(X), !,
  call(Goal_2, X, Y),
  path_closure_1(Goal_2, Y, Z, [X,Y], Path).
path_closure(Goal_2, X, Z, Path) :-
  ground(Z), !,
  call(Goal_2, Y, Z),
  path_closure_2(Goal_2, X, Y, [Y,Z], Path).
path_closure(_, X, Y, _) :-
  instantiation_error(args(X,Y)).



%! path_closure0(:Goal_2, +X, +Y) is semidet.
%! path_closure0(:Goal_2, +X, -Y) is nondet.
%! path_closure0(:Goal_2, -X, +Y) is nondet.
%! path_closure0(:Goal_2, +X, +Y, -Path:list) is semidet.
%! path_closure0(:Goal_2, +X, -Y, -Path:list) is nondet.
%! path_closure0(:Goal_2, -X, +Y, -Path:list) is nondet.
%
% Reflexive-transitive closure over X or Y.

path_closure0(Goal_2, X, Y) :-
  path_closure0(Goal_2, X, Y, _).


path_closure0(Goal_2, X, Y, Path) :-
  ground(X), !,
  path_closure_1(Goal_2, X, Y, [X], Path).
path_closure0(Goal_2, X, Y, Path) :-
  ground(Y), !,
  path_closure_2(Goal_2, X, Y, [Y], Path).
path_closure0(_, X, Y, _) :-
  instantiation_error(args(X,Y)).

path_closure_1(_, X, X, Path, Path).
path_closure_1(Goal_2, X, Z, Hist, Path) :-
  call(Goal_2, X, Y),
  \+ memberchk(Y, Hist),
  path_closure_1(Goal_2, Y, Z, [Y|Hist], Path).

path_closure_2(_, X, X, Path, Path).
path_closure_2(Goal_2, Z, X, Hist, Path) :-
  call(Goal_2, Y, X),
  \+ memberchk(Y, Hist),
  path_closure_2(Goal_2, Z, Y, [Y|Hist], Path).



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
