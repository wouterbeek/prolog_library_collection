:- module(
  graph_ext,
  [
    arcs_to_vertices/2, % +Arcs, -Vertices
    edges_to_vertices/2  % +Edges, -Vertices
  ]
).

/** <module> Generic support for graphs

*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).





%! arcs_to_vertices(+Arcs:list(compound), -Vertices:ordset) is det.

arcs_to_vertices(Arcs, Vertices) :-
  aggregate_all(
    set(Vertice),
    (
      member(arc(X,_,Y), Arcs),
      member(Vertice, [X,Y])
    ),
    Vertices
  ).



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
