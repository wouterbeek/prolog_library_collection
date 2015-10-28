:- module(
  betweenness,
  [
    betweenness/5 % +Graph
                  % :G2Vs_2
                  % :G2Es_2
                  % :V2Ns_2
                  % +Vertex
                  % -Betweenness:float
  ]
).

/** <module> Betweenness centrality

Predicate that calculate betweenness centrality.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(graph/graph_traverse)).
:- use_module(library(list_ext)).
:- use_module(library(pairs)).

:- meta_predicate(betweenness(+,2,2,2,-)).
:- meta_predicate(betweenness(+,2,2,2,+,-)).





%! betweenness(
%!   +Graph,
%!   :G2Vs_2,
%!   :G2Es_2,
%!   :V2Ns_2,
%!   -EdgeValues:list(pair)
%! ) is det.

betweenness(G, G2Vs_2, G2Es_2, V2Ns_2, EValues):-
  call(G2Vs_2, G, Vs),
  findall(
    Val-V,
    (
      member(V, Vs),
      betweenness(G, G2Vs_2, G2Es_2, V2Ns_2, V, Val)
    ),
    VValues
  ),
  call(G2Es_2, G, Es),
  findall(
    Sum-V1/V2,
    (
      member(V1-V2, Es),
      member(X-V1, VValues),
      member(Y-V2, VValues),
      Sum is X + Y
    ),
    UnsortedEValues
  ),
  keysort(UnsortedEValues, EValues).


%! betweenness(
%!   +Graph,
%!   :G2Vs_2,
%!   :G2Es_2,
%!   :V2Ns_2,
%!   +Vertex,
%!   -Betweenness:float
%! ) is det.
% Betweenness centrality.

betweenness(G, G2Vs_2, G2Es_2, V2Ns_2, V, Betweenness):-
  call(G2Vs_2, G, Vs),
  aggregate_all(
    sum(Betweenness),
    (
      member(From, To, Vs),
      shortest_paths(G, G2Es_2, V2Ns_2, From, To, GenericShortestPaths),
      length(GenericShortestPaths, NumberOfGenericShortestPaths),
      shortest_paths(G, G2Es_2, V2Ns_2, From, V, SpecificShortestPaths1),
      shortest_paths(G, G2Es_2, V2Ns_2, V, To, SpecificShortestPaths2),
      maplist(
        append,
        SpecificShortestPaths1,
        SpecificShortestPaths2,
        SpecificShortestPaths
      ),
      length(SpecificShortestPaths, NumberOfSpecificShortestPaths),
      Betweenness is NumberOfSpecificShortestPaths / NumberOfGenericShortestPaths
    ),
    Betweenness
  ).





% HELPERS %

shortest_paths(G, G2Es, V2Ns, V, W, ShortestPaths):-
  aggregate_all(
    set(Length-Path),
    (
      traverse(
        G,
        G2Es,
        V2Ns,
        V,
        W,
        _,
        _,
        Path,
        [distinct_vertices(true),vertex_distance(Length)]
      )
    ),
    Pairs
  ),
  group_pairs_by_key(Pairs, Joined),
  (   Joined == []
  ->  ShortestPaths = []
  ;   Joined = [_-ShortestPaths|_]
  ).
