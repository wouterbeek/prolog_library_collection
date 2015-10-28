:- module(
  s_vertex,
  [
    s_adjacent_vertices/3, % +Graph:ugraph
                           % ?Vertex1
                           % ?Vertex2
    s_dominating_vertex/2, % +Graph:ugraph
                           % ?Dominating
    s_in_degree/3, % +Graph:ugraph
                   % ?Vertex
                   % ?InDegree:nonneg
    s_neighbor/3, % +Graph:ugraph
                  % ?Vertex
                  % ?Neighbor
    s_out_degree/3, % +Graph:ugraph
                    % +Vertex
                    % ?OutDegree:nonneg
    s_vertex/2, % +Graph:ugraph
                % ?Vertex
    s_vertex_degree/3 % +Graph:ugraph
                      % ?Vertex
                      % -Degree:nonneg
  ]
).

/** <module> Graph theory: Vertices

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(graph/s/s_test)).
:- use_module(library(lists)).
:- use_module(library(plunit)).





%! s_adjacent_vertices(+Graph:ugraph, +V, +W) is semidet.
%! s_adjacent_vertices(+Graph:ugraph, +V, -W) is nondet.
%! s_adjacent_vertices(+Graph:ugraph, -V, +W) is nondet.
%! s_adjacent_vertices(+Graph:ugraph, -V, -W) is nondet.
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
% ?- s_adjacent_vertices([1-[2]], X, Y).
% X = 1,
% Y = 2.
% ?- s_adjacent_vertices([1-[2],2-[1]], X, Y).
% X = 1,
% Y = 2 ;
% false.
% ```
%
% This predicate guarantees that each pair of adjacent vertices
% is returned exactly once.
% This is why in the above example result set \set{X=1,Y=2}
% occurs exactly once, even in cases where edge \pair{1}{2} is undirected.

s_adjacent_vertices(G, V, W):-
  maplist(nonvar, [V,W]),
  V @> W, !,
  s_adjacent_vertices(G, W, V).
s_adjacent_vertices(G, V, W):-
  s_edge(G, V0-W0),
  (    V0 @< W0
  ->   V = V0,
       W = W0
  ;    \+ s_edge(G, W0-V0)
  ->   V = W0,
       W = V0
  ).

:- begin_tests('s_adjacent_vertices/3').

test(
  's_adjacent_vertices(+,-,-) is nondet. TRUE',
  % We know the exact order in which the results should be returned.
  [forall(s_adjacent_vertices_test(GName,VPairs,true))]
):-
  s_graph_test(GName, G),
  findall(V-W, s_adjacent_vertices(G, V, W), VPairs).

s_adjacent_vertices_test(various(1), [1-3,2-3,2-4,2-5,3-4,3-5], true).

:- end_tests('s_adjacent_vertices/3').



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

  % s_adjacent_vertices/4 will reorder the arguments
  % in case `V > W`.
  maplist(s_adjacent_vertices(G, V), Ws).

:- begin_tests('s_dominating_vertex/2').

test(
  's_dominating_vertex(+,+) is semidet. TRUE',
  [forall(s_dominating_vertex_test(GName,V, true))]
):-
  s_graph_test(GName, G),
  s_dominating_vertex(G, V).
test(
  's_dominating_vertex(+,-) is nondet. TRUE',
  [all(V == [3])]
):-
  s_graph_test(various(1), G),
  s_dominating_vertex(G, V).

s_dominating_vertex_test(various(1), 3, true).

:- end_tests('s_dominating_vertex/2').



%! s_in_degree(+Graph:ugraph, +V, +InDegree:nonneg) is semidet.
%! s_in_degree(+Graph:ugraph, +V, -InDegree:nonneg) is det.
%! s_in_degree(+Graph:ugraph, -V, +InDegree:nonneg) is nondet.
%! s_in_degree(+Graph:ugraph, -V, -InDegree:nonneg) is nondet.

s_in_degree(G, V, InDegree):-
  s_vertex(G, V),
  aggregate_all(
    count,
    s_edge(G, _-V),
    InDegree
  ).



%! s_neighbor(+Graph:ugraph, +V, +W) is semidet.
%! s_neighbor(+Graph:ugraph, +V, -W) is nondet.
%! s_neighbor(+Graph:ugraph, -V, +W) is nondet.
%! s_neighbor(+Graph:ugraph, -V, -W) is nondet.
% Respects directionality.

s_neighbor(G, V, W):-
  s_edge(G, V-W).



%! s_out_degree(+Graph:ugraph, +V, +InDegree:nonneg) is semidet.
%! s_out_degree(+Graph:ugraph, +V, -InDegree:nonneg) is det.
%! s_out_degree(+Graph:ugraph, -V, +InDegree:nonneg) is nondet.
%! s_out_degree(+Graph:ugraph, -V, -InDegree:nonneg) is nondet.

s_out_degree(G, V, OutDegree):-
  s_vertex(G, V),
  aggregate_all(count, s_edge(G, _-V), OutDegree).



%! s_vertex(+G, +V) is semidet.
%! s_vertex(+G, -V) is nondet.

s_vertex(G, V):-
  nonvar(V), !,
  memberchk(V-_, G).
s_vertex(G, V):-
  member(V-_, G).



%! s_vertex_degree(+Graph:ugraph, +V, +Degree:nonneg) is semidet.
%! s_vertex_degree(+Graph:ugraph, +V, -Degree:nonneg) is det.
%! s_vertex_degree(+Graph:ugraph, -V, +Degree:nonneg) is nondet.
%! s_vertex_degree(+Graph:ugraph, -V, -Degree:nonneg) is nondet.
% The *degree* of a vertex is the summation, for each edge, of
% the number of times the vertex occurs in the edge.
%
% This means that a reflective edge, if present,
% is counted twice in calculating the vertex degree.

s_vertex_degree(G, V, Degree):-
  s_vertex(G, V),
  s_neighbors(V, G, Ws),
  length(Ws, Degree0),
  
  % Correct for reflexive edges, which are counted twice.
  (memberchk(V, Ws) -> Degree is Degree0 + 1 ; Degree = Degree0).

:- begin_tests('s_vertex_degree/3').

test(
  's_vertex_degree(+,+,+) is semidet. TRUE',
  [forall(s_vertex_degree_test(G,V,Degree,true))]
):-
  s_vertex_degree(G, V, Degree).
test(
  's_vertex_degree(+,+,+) is semidet. FAIL',
  [fail,forall(s_vertex_degree_test(G,V,Degree,fail))]
):-
  s_vertex_degree(G, V, Degree).
test(
  's_vertex_degree(+,+,-) is det. TRUE',
  [all(V-Degree == [1-1,2-3,3-4,4-2,5-2])]
):-
  s_graph_test(various(1), G),
  s_vertex_degree(G, V, Degree).

s_vertex_degree_test(G, V, Degree, true):-
  s_graph_test(various(1), G),
  member(V-Degree, [1-1,2-3,3-4,4-2,5-2]).
s_vertex_degree_test(G, V, Degree, fail):-
  s_graph_test(various(1), G),
  member(V-Degree, [1-0,2-1,3-12,4-3,5-4]).

:- end_tests('s_vertex_degree/3').
