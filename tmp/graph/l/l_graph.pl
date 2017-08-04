:- module(
  l_graph,
  [
    l_edges_vertices/2 % +Es, -Vs
  ]
).

/** <module> Labeled graphs

Support for graphs with labeled edges.

@author Wouter Beek
@version 2015/10, 2015/12, 2016/02
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).





%! l_edges_vertices(+Edges:list(compound), -Vertices:ordset) is det.

l_edges_vertices(Es, Vs):-
  aggregate_all(set(V), (member(edge(X,_,Y), Es), member(V, [X,Y])), Vs).
