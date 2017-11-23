:- module(
  graph_ext,
  [
    edges_to_vertices/2 % +Edges, -Vertices
  ]
).

/** <module> Graph extensions

@author Wouter Beek
@version 2017/11
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).





%! edges_to_vertices(+Edges:list(compound), -Vertices:ordset) is det.

edges_to_vertices(Edges, Vertices) :-
  aggregate_all(
    set(Vertice),
    (
      member(edge(X,_,Y), Edges),
      member(Vertice, [X,Y])
    ),
    Vertices
  ).
