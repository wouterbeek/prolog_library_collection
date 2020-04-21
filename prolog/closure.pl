:- module(
  closure,
  [
    leave_closure/3,  % :Goal_2, ?X, ?Y
    leave_closure0/3, % :Goal_2, ?X, ?Y
    path_closure/3,   % :Goal_2, ?X, ?Y
    path_closure/4,   % :Goal_2, ?X, ?Y, ?Path
    path_closure0/3,  % :Goal_2, ?X, ?Y
    path_closure0/4,  % :Goal_2, ?X, ?Y, ?Path
    path_distance/4   % :Goal_2, ?X, ?Y. ?Distance
  ]
).

/** <module> Closures

Generic closure predicates that operate over graph data structures.

*/

:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(plunit)).

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
%! path_closure(:Goal_2, +X, +Y, +Path:list) is semidet.
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
  X \== Y,
  path_closure_1(Goal_2, Y, Z, [Y,X], Path0),
  reverse(Path0, Path).
path_closure(Goal_2, X, Z, Path) :-
  ground(Z), !,
  call(Goal_2, Y, Z),
  Y \== Z,
  path_closure_2(Goal_2, X, Y, [Z,Y], Path0),
  reverse(Path0, Path).
path_closure(_, X, Y, _) :-
  instantiation_error(args(X,Y)).



%! path_closure0(:Goal_2, +X, +Y) is semidet.
%! path_closure0(:Goal_2, +X, -Y) is nondet.
%! path_closure0(:Goal_2, -X, +Y) is nondet.
%! path_closure0(:Goal_2, +X, +Y, +Path:list) is semidet.
%! path_closure0(:Goal_2, +X, +Y, -Path:list) is semidet.
%! path_closure0(:Goal_2, +X, -Y, -Path:list) is nondet.
%! path_closure0(:Goal_2, -X, +Y, -Path:list) is nondet.
%
% Reflexive-transitive closure over X or Y.

path_closure0(Goal_2, X, Y) :-
  path_closure0(Goal_2, X, Y, _).


path_closure0(Goal_2, X, Y, Path) :-
  ground(X), !,
  path_closure_1(Goal_2, X, Y, [X], Path0),
  reverse(Path0, Path).
path_closure0(Goal_2, X, Y, Path) :-
  ground(Y), !,
  path_closure_2(Goal_2, X, Y, [Y], Path0),
  reverse(Path0, Path).
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



%! path_distance(:Goal_2, +X, +Y, +Distance:nonneg) is semidet.
%! path_distance(:Goal_2, +X, +Y, -Distance:nonneg) is semidet.
%! path_distance(:Goal_2, +X, -Y, -Distance:nonneg) is nondet.
%! path_distance(:Goal_2, -X, +Y, -Distance:nonneg) is nondet.

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





% TESTS %

:- begin_tests(closure).

test(leave_closure, [set(Y==[charing_cross])]) :-
  leave_closure(arc(1), bond_street, Y).

test(path_closure, [set(Y-P == [b-[a,b],c-[a,b,c]])]) :-
  path_closure(arc(2), a, Y, P).

test(path_closure0, [set(Y-P == [a-[a],b-[a,b],c-[a,b,c]])]) :-
  path_closure0(arc(2), a, Y, P).

arc(1, bond_street, oxford_circus).
arc(1, oxford_circus, tottenham_court_road).
arc(1, bond_street, green_park).
arc(1, green_park, charing_cross).
arc(1, green_park, piccadilly_circus).
arc(1, piccadilly_circus, leicester_square).
arc(1, green_park, oxford_circus).
arc(1, oxford_circus, piccadilly_circus).
arc(1, piccadilly_circus, charing_cross).
arc(1, tottenham_court_road, leicester_square).
arc(1, leicester_square, charing_cross).
arc(2, a, a).
arc(2, a, b).
arc(2, b, c).

:- end_tests(closure).
