:- module(
  ugraphs_ext,
  [
    connected_vertices/2 % +UGraph, -SetsOfVertices
  ]
).

/** <module> Extensions to the UGRAPHS library

*/

:- reexport(library(ugraphs)).





%! connected_vertices(+UGraph, -SetsOfVertices) is det.
%
% Partition the vertices in UGraph based on the reachability closure
% over edges.

connected_vertices([], []) :- !.
connected_vertices(G1, [Set|Sets]) :-
  G1 = [V-_|_],
  reachable(V, G1, Set),
  del_vertices(G1, Set, G2),
  connected_vertices(G2, Sets).
