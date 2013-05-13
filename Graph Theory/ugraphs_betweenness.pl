:- module(
  ugraphs_betweenness,
  [
    betweenness/2 % +UGraph
                  % -SortedEdges:list
  ]
).

/** <module> UGraphs betweenness

# Sample ugraph

[1-[2,3], 2-[1,3], 3-[1,2,4], 4-[3,5,6], 5-[4,6], 6-[4,5]]

[1-[2,3], 2-[3], 3-[4], 4-[5,6], 5-[6], 6-[]]

@author Wouter Beek
@version 2013/03
*/

:- use_module(generics(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs)).



betweenness(UGraph, SortedEdgeSums):-
  vertices(UGraph, Vertices),
  edges(UGraph, Edges),
  findall(
    VertexBetweenness-Vertex,
    (
      member(Vertex, Vertices),
      findall(
        VertexSum,
        (
          member(From, To, Vertices),
          From \== To,
          shortest_paths(UGraph, From, To, _Cross, M-_),
          shortest_paths(UGraph, From, To, Vertex, N-_),
          VertexSum is N / M
        ),
        VertexSums
      ),
      sum_list(VertexSums, VertexBetweenness)
    ),
    VerticesBetweenness
  ),
  findall(
    EdgeSum-V-W,
    (
      member(V-W, Edges),
      member(SumV-V, VerticesBetweenness),
      member(SumW-W, VerticesBetweenness),
      EdgeSum is SumV + SumW
    ),
    EdgeSums
  ),
  predsort(icompare, EdgeSums, SortedEdgeSums).

icompare(InvertedOrder, Term1, Term2):-
  compare(Order, Term1, Term2),
  i(Order, InvertedOrder).

i(<, >).
i(>, <).
i(=, =).

shortest_paths(UGraph, From, To, Cross, ShortestLength-ShortestPath):-
  findall(
    Length-Path,
    (
      travel(UGraph, From, To, Length, Vertices, Path),
      member(Cross, Vertices)
    ),
    Pairs1
  ),
  sort(Pairs1, Pairs2), % Why?!
  group_pairs_by_key(Pairs2, Sorted),
  (
    Sorted == []
  ->
    ShortestLength = 0,
    ShortestPath = []
  ;
    Sorted = [ShortestLength-ShortestPath | _]
  ).

travel(UGraph, From, To, Length, AllVertices, Path):-
  travel(UGraph, From, To, Length, [From], AllVertices, Path).

travel(_UGraph, To, To, Length, AllVertices, AllVertices, [To]):-
  length(AllVertices, Length),
  !.
travel(UGraph, From, To, Length, Vertices, AllVertices, [From | Path]):-
  neighbors(From, UGraph, Tos),
  member(X, Tos),
  \+ member(X, Vertices),
  ord_add_element(Vertices, X, NewVertices),
  travel(UGraph, X, To, Length, NewVertices, AllVertices, Path).

