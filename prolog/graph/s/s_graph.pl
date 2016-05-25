:- module(
  s_graph,
  [
    s_adjacent_edge/3, % +Graph:ugraph
                       % ?Edge1:pair
                       % ?Edge2:pair
    s_adjacent_vertex/3, % +Graph:ugraph
                         % ?Vertex1
                         % ?Vertex2
    s_dominating_vertex/2, % +Graph:ugraph
                           % ?Dominating
    s_edge/2, % +Graph:ugraph
              % ?Edge:pair
    s_edges/2, % ?Graph:ugraph
               % ?Edges:ordset(pair)
    s_graph_components/3, % ?Graph:ugraph
                          % ?Vertices:ordset
                          % ?Edges:ordset(pair) 
    s_head/2, % +Graph:ugraph
              % ?Head
    s_tail/2, % +Graph:ugraph
              % ?Tail
    s_link/2, % +Graph:ugraph
              % ?Link:pair
    s_loop/2, % +Graph:ugraph
              % ?Loop:pair
    s_neighbor/3, % +Graph:ugraph
                  % ?Vertex
                  % ?Neighbor
    s_unsymmetric_edge/2, % +Graph:ugraph
                          % ?Edge:pair
    s_vertex/2 % +Graph:ugraph
               % ?Vertex
 ]
).
:- reexport(
  library(ugraphs),
  [
    add_edges/3 as s_add_edges, % +Graph:ugraph
                                % +Edges:ordset(pair)
                                % -NewGraph:ugraph
    add_vertices/3 as s_add_vertives, % +Graph:ugraph
                                      % +Vertices:ordset
                                      % -NewGraph:ugraph
    complement/2 as s_complement, % +Graph:ugraph
                                  % -NewGraph:ugraph
    compose/3 as s_compose, % +LeftGraph:ugraph
                            % +RightGraph:ugraph
                            % -NewGraph:ugraph
    del_edges/3 as s_del_edges, % +Graph:ugraph
                                % +Edges:ordset(pair)
                                % -NewGraph:ugraph
    del_vertices/3 as s_del_vertices, % +Graph:ugraph
                                      % +Vertices:ordset
                                      % -NewGraph:ugraph
    neighbors/3 as s_neighbors, % +Vertex
                                % +Graph:ugraph
                                % -Neighbors:ordset
    reachable/3 as s_reachables, % +Vertex
                                 % +Graph:ugraph
                                 % -Reachables:ordset
    top_sort/2 as s_top_sort, % +Graph:ugraph
                              % -Sort:list
    transitive_closure/2 as s_transitive_closure, % +Graph:ugraph
                                                  % -ClosureGraph:ugraph
    transpose_ugraph/2 as s_transpose, % +Graph:ugraph
                                       % -NewGraph:ugraph
    ugraph_union/3 as s_union, % +Graph1:ugraph
                               % +Graph2:ugraph
                               % -NewGraph:ugraph
    vertices/2 as s_vertices % +Graph:ugraph
                             % -Vertices:ordset
  ]
).

/** <module> S-representation for graphs

Support for the S-representation of graphs.
This mostly consists of the graph library that is included in SWI-Prolog
 and that was written by Richard O'Keefe and Vitor Santos Costa.

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(graph/s/s_edge)).
:- use_module(library(graph/s/s_test)).
:- use_module(library(lists)).
:- use_module(library(pair_ext)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).
:- use_module(library(ugraphs), [
     edges/2 as edges0,
     vertices_edges_to_ugraph/3 as vertices_edges_to_ugraph0
   ]).

:- multifile(error:has_type/2).
error:has_type(ugraph, X):-
  error:has_type(list(pair(any,list)), X).





%! s_adjacent_edge(+Graph:ugraph, +Edge1:pair, +Edge2:pair) is semidet.
%! s_adjacent_edge(+Graph:ugraph, +Edge1:pair, -Edge2:pair) is nondet.
%! s_adjacent_edge(+Graph:ugraph, -Edge1:pair, +Edge2:pair) is nondet.
%! s_adjacent_edge(+Graph:ugraph, -Edge1:pair, -Edge2:pair) is nondet.
% Returns all an only adjacent edges.
%
% Edges are *adjacent* iff they share exactly one vertex.
%
% No guarentees are given about the order in which results are returned.

% Case 1: symmetric edges.
s_adjacent_edge(G, V-W, W-V):-
  s_edge(G, V-W),
  s_edge(G, W-V),
  V \== W.
% Case 2: consecutive edges - forward
s_adjacent_edge(G, W1-V, V-W2):-
  s_edge(G, W1-V),
  s_edge(G, V-W2),
  W1 \== W2.
% Case 3: consecutive edges - backward
s_adjacent_edge(G, V-W1, W2-V):-
  s_edge(G, V-W1),
  s_edge(G, W2-V),
  W1 \== W2.
% Case 4: diverging edges
s_adjacent_edge(G, V-W1, V-W2):-
  s_edge(G, V-W1),
  s_edge(G, V-W2),
  W1 \== W2.
% Case 5: converging edges
s_adjacent_edge(G, W1-V, W2-V):-
  s_edge(G, W1-V),
  s_edge(G, W2-V),
  W1 \== W2.

:- begin_tests('s_adjacent_edge/3').

test(
  's_adjacent_edge(+,+,-) is nondet. TRUE',
  [forall(s_adjacent_edge_test(GName,Es,true))]
):-
  s_test_graph(GName, G),
  aggregate_all(set(E), s_adjacent_edge(G, 1-2, E), Es).

s_adjacent_edge_test(path(3), [2-1,2-3,3-2], true).

:- end_tests('s_adjacent_edge/3').



%! s_adjacent_vertex(+Graph:ugraph, +V, +W) is semidet.
%! s_adjacent_vertex(+Graph:ugraph, +V, -W) is nondet.
%! s_adjacent_vertex(+Graph:ugraph, -V, +W) is nondet.
%! s_adjacent_vertex(+Graph:ugraph, -V, -W) is nondet.
% Vertices are *adjacent* iff there is an edge for which they are
% the endpoints.
%
% Notice that adjacency does not respect the directedness of edges.
%
% Adjacent vertices are returned according to the normal ordering on terms.
% For example, if [1] is an undirected edge,
% then instantiation [2] will occur, but instantiation [3] will not.
%
% ```latex
% [1]   \pair{a}{b}
% [2]   \set{V=a,W=b}
% [3]   \set{V=b,W=a}
% ```
%
% This results in the following behavior:
%
% ```prolog
% ?- s_adjacent_vertex([1-[2]], X, Y).
% X = 1,
% Y = 2.
% ?- s_adjacent_vertex([1-[2],2-[1]], X, Y).
% X = 1,
% Y = 2 ;
% false.
% ```
%
% This predicate guarantees that each pair of adjacent vertices
% is returned exactly once.
% This is why in the above example result set \set{X=1,Y=2}
% occurs exactly once, even in cases where edge \pair{1}{2} is undirected.

s_adjacent_vertex(G, V, W):-
  maplist(nonvar, [V,W]),
  V @> W, !,
  s_adjacent_vertex(G, W, V).
s_adjacent_vertex(G, V, W):-
  s_edge(G, V0-W0),
  (    V0 @< W0
  ->   V = V0,
       W = W0
  ;    \+ s_edge(G, W0-V0)
  ->   V = W0,
       W = V0
  ).

:- begin_tests('s_adjacent_vertex/3').

test(
  's_adjacent_vertex(+,-,-) is nondet. TRUE',
  % We know the exact order in which the results should be returned.
  [forall(s_adjacent_vertex_test(GName,VPairs,true))]
):-
  s_test_graph(GName, G),
  findall(V-W, s_adjacent_vertex(G, V, W), VPairs).

s_adjacent_vertex_test(various(1), [1-3,2-3,2-4,2-5,3-4,3-5], true).

:- end_tests('s_adjacent_vertex/3').



%! s_dominating_vertex(+Graph:ugraph, +V) is semidet.
%! s_dominating_vertex(+Graph:ugraph, -V) is nondet.
% A *dominating* vertex is one that is adjacent to every _other_ vertex.

s_dominating_vertex(G, V):-
  nonvar(V), !,
  s_dominating_vertex0(G, V), !.
s_dominating_vertex(G, V):-
  s_dominating_vertex0(G, V).

s_dominating_vertex0(G, V):-
  s_vertices(G, Vs),

  % Extract the set of *other* vertices.
  select(V, Vs, Ws),

  % s_adjacent_vertex/4 will reorder the arguments
  % in case `V > W`.
  maplist(s_adjacent_vertex(G, V), Ws).

:- begin_tests('s_dominating_vertex/2').

test(
  's_dominating_vertex(+,+) is semidet. TRUE',
  [forall(s_dominating_vertex_test(GName,V, true))]
):-
  s_test_graph(GName, G),
  s_dominating_vertex(G, V).
test(
  's_dominating_vertex(+,-) is nondet. TRUE',
  [all(V == [3])]
):-
  s_test_graph(various(1), G),
  s_dominating_vertex(G, V).

s_dominating_vertex_test(various(1), 3, true).

:- end_tests('s_dominating_vertex/2').



%! s_edge(+Graph:ugraph, +Edge:pair) is semidet.
%! s_edge(+Graph:ugraph, -Edge:pair) is nondet.

s_edge(G, Tail-Head):- member(Tail-Heads, G), member(Head, Heads).



%! s_edges(+Graph:ugraph, -Edges:ordset(pair)) is det.
%! s_edges(-Graph:ugraph, +Edges:ordset(pair)) is det.

s_edges(G, Es):- nonvar(G), !, edges0(G, Es).
s_edges(G, Es):- s_edges_vertices(Es, Vs), s_graph_components(G, Vs, Es).



%! s_graph_components(
%!   +Graph:ugraph,
%!   -Vertices:ordset,
%!   -Edges:ordset(pair)
%! ) is det.
%! s_graph_components(
%!   -Graph:ugraph,
%!   +Vertices:ordset,
%!   +Edges:ordset(pair)
%! ) is det.
% Decomposes/composes a graph into/based in its vertices and edges.

s_graph_components(G, Vs, Es):-
  var(G), !,
  (var(Vs) -> s_edges_vertices(Es, Vs) ; true),
  vertices_edges_to_ugraph0(Vs, Es, G).
s_graph_components(G, Vs, Es):-
  s_vertices(G, Vs),
  s_edges(G, Es).



%! s_head(+Graph:ugraph:ugraph, +Head) is semidet.
%! s_head(+Graph:ugraph:ugraph, -Head) is nondet.

s_head(G, Head):- s_edge(G, _-Head).



%! s_link(+Graph:ugraph, +Link:pair) is semidet.
%! s_link(+Graph:ugraph, -Link:pair) is nondet.
% A *link* is a non-reflexive edge.
%
% Optimized for the semi-deterministic case where `Link` is a loop.

s_link(G, V-W):- s_edge(G, V-W), V \== W.



%! s_loop(+Graph:ugraph, +Loop:pair) is semidet.
%! s_loop(+Graph:ugraph, -Loop:pair) is nondet.
% A *loop* is a reflexive edge.

s_loop(G, V-V):- s_edge(G, V-V).



%! s_neighbor(+Graph:ugraph, +V, +W) is semidet.
%! s_neighbor(+Graph:ugraph, +V, -W) is nondet.
%! s_neighbor(+Graph:ugraph, -V, +W) is nondet.
%! s_neighbor(+Graph:ugraph, -V, -W) is nondet.
% Respects directionality.

s_neighbor(G, V, W):-
  s_edge(G, V-W).



%! s_tail(+Graph:ugraph, +Tail) is semidet.
%! s_tail(+Graph:ugraph, -Tail) is nondet.

s_tail(G, Tail):- s_edge(G, Tail-_).



%! s_unsymmetric_edge(+Graph:ugraph, +Edge:pair) is semidet.
%! s_unsymmetric_edge(+Graph:ugraph, -Edge:pair) is nondet.
% For every pair of symmetric edges $\set{\tuple{V, W}, \tuple{W, V}}$
% we only take the edge for which the first member is smaller than the
% latter.
%
% *|Special case|*: Reflexive edges are symmetric and therefore removed
%                   entirely.

s_unsymmetric_edge(G, V-W):- s_edge(G, V-W), \+ s_edge(G, W-V).



%! s_vertex(+G, +V) is semidet.
%! s_vertex(+G, -V) is nondet.

s_vertex(G, V):- member(V-_, G).
