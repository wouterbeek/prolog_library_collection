:- module(
  intersection,
  [
    intersections/2 % +BaseSets:list(ordset)
                    % -AllIntersections:list(ordset)
  ]
).

/** <module> Derive all set intersections

@author Wouter Beek
@verion 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).





%! intersection(+BaseSets:list(ordset), -AllIntersections:list(ordset)) is det.

intersections(Ss1, Sol):-
  intersections(Ss1, Ss1, Ss1, Sol).

intersections(Ss1, Ss2, Hist1, Sol):-
  aggregate_all(
    set(S3),
    (
      member(S1, Ss1),
      member(S2, Ss2),
      ord_intersection(S1, S2, S3),
      \+ member(S3, Hist1)
    ),
    Ss3
  ),
  (   Ss3 == []
  ->  Sol = Hist1
  ;   ord_union(Hist1, Ss3, Hist2),
      intersections(Ss1, Ss3, Hist2, Sol)
  ).
