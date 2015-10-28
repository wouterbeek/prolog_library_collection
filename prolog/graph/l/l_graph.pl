:- module(
  l_graph,
  [
    l_edges_vertices/2 % +Edges:list(compound)
                       % -Vertices:ordset
  ]
).

/** <module> Labeled graphs

Support for graphs with labeled edges.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(lists)).





%! l_edges_vertices(+Es:list(compound), -Vs:ordset) is det.

l_edges_vertices(Es,Vs):-
  aggregate_all(
    set(V),
    (
      member(edge(X,_,Y), Es),
      member(V, [X,Y])
    ),
    Vs
  ).
