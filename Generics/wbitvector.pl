:- module(
  wbitvector,
  [
    list_map/2, % +Integers:list(integer)
                % -Map:bitvector
    map_count/2, % +Map:bitvector
                 % -Count:integer
    map_difference/3, % +Map:bitvector
                      % +Delete:bitvector
                      % -Difference:bitvector
    map_intersection/3, % +Map1:bitvector
                        % +Map2:bitvector
                        % -Intersection:bitvector
    map_list/2, % +Map:bitvector
                % -Integers:list(integer)
    map_replace/4, % +Clear:set(integer)
                   % +Set:set(integer)
                   % +Map:bitvector
                   % -Map:bitvector
    map_union/3, % +Map1:bitvector
                 % +Map2:bitvector
                 % -Union:bitvector
    map_union_unique/3, % +Map1:bitvector
                        % +Map2:bitvector
                        % -Union:bitvector
    map_without_intersection/4 % +Map1:bitvector
                               % +Map2:bitvector
                               % -Map3:bitvector
                               % -Map4:bitvector
  ]
).

/** <module> Bitvector

Bitvector library.

*/



%% list_map(+Integers:list(integer), -Map:bitvector)
% Convert the given list of integers into a bit-vector.

list_map(Integers, bitvector(Map)):-
  list_map_(Integers, Map).

list_map_([], 0).
list_map_([Integer | Integers], Map):-
  list_map_(Integers, PartialMap),
  Map is PartialMap \/ (1 << Integer).

%% map_count(+Map:bitvector, -Count:number)
% Return the number of 1s in the bitvector.

map_count(bitvector(A), Count):-
  Count is popcount(A).

%% map_difference(+Map:bitvector, +Delete:bitvector, -Diff:bitvector) is det.
% Returns the first bitvector without the elements from the second bitvector.

map_difference(bitvector(A), bitvector(B), bitvector(C)):-
  C is A xor B.

%% map_intersection(
%%   +Map1:bitvector,
%%   +Map2:bitvector,
%%   -Intersection:bitvector
%% ) is det.

map_intersection(bitvector(A), bitvector(B), bitvector(C)):-
  C is A /\ B.

%% map_list(+Map, -Integers)
% Convert a bit-vector into a sorted list of integers.

map_list(bitvector(Map), Integers):-
  map_list_(Map, Integers).

%% map_list_(+Map:bitvector, -Integers:list(integer)) is det.

map_list_(0, []):-
  !.
map_list_(Map, [Integer | Integers]):-
  % Return the smallest integer N such that (IntExpr >> N) /\ 1 =:= 1.
  % This is the (zero-origin) index of the least significant 1 bit in
  % the value of IntExpr, which must evaluate to a positive integer.
  % Errors for 0, negative integers, and non-integers.
  Integer is lsb(Map),
  PartialMap is Map /\ \(1 << Integer),
  map_list_(PartialMap, Integers).

%% map_replace(+Clear, +Set, +Map1:bitvector, -Map:bitvector) is det.
% Replace Clear by Set in Map1

map_replace(Clear, Set, bitvector(A), bitvector(B)):-
  % This fails if ??? is not a set.
  0 =\= A /\ (1<<Clear),
  B is (A /\ \(1<<Clear)) \/ (1<<Set).

%% map_union(+Map1:bitvector, +Map2:bitvector, -Union:bitvector) is det.
% Compute the set-union of two maps

map_union(bitvector(A), bitvector(B), bitvector(C)):-
  C is A \/ B.

%% map_union_unique(
%%   +Map1:bitvector,
%%   +Map2:bitvector,
%%   -Union:bitvector
%% ) is det.
% Like union, but succeed only if the intersection is empty.

map_union_unique(bitvector(A), bitvector(B), bitvector(C)):-
  A /\ B =:= 0,
  C is A \/ B.

%  map_without_intersection(+Map1, +Map2, -Map3, -Map4)
%
%  Remove intersection of both maps from both maps; fail if no
%  intersection

map_without_intersection(bitvector(A), bitvector(B),
       bitvector(C), bitvector(D)):-
  Intersection is A /\ B,
  Intersection \== 0, % FL nov 07 used to be =\= but arithmetic is not necessary here (and slower!)
  C is A xor Intersection,
  D is B xor Intersection.

:- multifile
  user:portray/1.

user:portray(bitvector(Map)):-
  integer(Map),
  map_list_(Map, Integers),
  format('bitvector(~p)', [Integers]).
