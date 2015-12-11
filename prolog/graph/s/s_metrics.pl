:- module(
  s_metrics,
  [
    s_degree/2, % +Graph, -Degree
    s_degree/3, % +Graph:ugraph
                % ?Vertex
                % -Degree:nonneg
    s_degree_sequence/2, % +Graph:ugraph
                         % -DegreeSequence:list(nonneg)
    s_indegree/3, % +Graph:ugraph
                  % ?Vertex
                  % ?InDegree:nonneg
    s_max_degree/2, % +Graph:ugraph
                    % ?Degree:nonneg
    s_min_degree/2, % +Graph:ugraph
                    % ?Degree:nonneg
    s_order/2, % +Graph:ugraph
               % ?Order:nonneg
    s_outdegree/3, % +Graph:ugraph
                   % +Vertex
                   % ?OutDegree:nonneg
    s_size/2 % +Graph:ugraph
             % ?Size:nonneg
  ]
).

/** <module> Graph theory: Metrics

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(graph/s/s_subgraph)).
:- use_module(library(graph/s/s_test)).
:- use_module(library(graph/s/s_type)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(plunit)).





%! s_degree(+Graph:ugraph, +Degree:nonneg) is semidet.
%! s_degree(+Graph:ugraph, -Degree:nonneg) is semidet.
% The *degree* of a graph is the degree of each of its vertices.
%
% This fails silently for non-regular graphs (which do not have a degree).

s_degree(G, Degree):-
  s_vertex(G, V1),
  s_degree(G, V1, Degree),
  forall(s_vertex(G, V), s_degree(G, V, Degree)).
s_degree(_, 0).



%! s_degree(+Graph:ugraph, +V, +Degree:nonneg) is semidet.
%! s_degree(+Graph:ugraph, +V, -Degree:nonneg) is det.
%! s_degree(+Graph:ugraph, -V, +Degree:nonneg) is nondet.
%! s_degree(+Graph:ugraph, -V, -Degree:nonneg) is nondet.
% The *degree* of a vertex is the summation, for each edge, of
% the number of times the vertex occurs in the edge.
%
% This means that a reflective edge, if present,
% is counted twice in calculating the vertex degree.

s_degree(G, V, Degree):-
  s_vertex(G, V),
  s_neighbors(V, G, Ws),
  length(Ws, Degree0),
  
  % Correct for reflexive edges, which are counted twice.
  (memberchk(V, Ws) -> Degree is Degree0 + 1 ; Degree = Degree0).

:- begin_tests('s_degree/3').

test(
  's_degree(+,+,+) is semidet. TRUE',
  [forall(s_degree_test(G,V,Degree,true))]
):-
  s_degree(G, V, Degree).
test(
  's_degree(+,+,+) is semidet. FAIL',
  [fail,forall(s_degree_test(G,V,Degree,fail))]
):-
  s_degree(G, V, Degree).
test(
  's_degree(+,+,-) is det. TRUE',
  [all(V-Degree == [1-1,2-3,3-4,4-2,5-2])]
):-
  s_test_graph(various(1), G),
  s_degree(G, V, Degree).

s_degree_test(G, V, Degree, true):-
  s_test_graph(various(1), G),
  member(V-Degree, [1-1,2-3,3-4,4-2,5-2]).
s_degree_test(G, V, Degree, fail):-
  s_test_graph(various(1), G),
  member(V-Degree, [1-0,2-1,3-12,4-3,5-4]).

:- end_tests('s_degree/3').



%! s_degree_sequence(+Graph:ugraph, +DegreeSequence:list(nonneg)) is semidet.
%! s_degree_sequence(+Graph:ugraph, -DegreeSequence:list(nonneg)) is det.

s_degree_sequence(G, DegreeSeq):-
  s_vertices(G, Vs),
  maplist(\V^Degree^s_degree(G, V, Degree), Vs, VDegrees),
  sort(0, @>=, VDegrees, DegreeSeq).

:- begin_tests('s_degree_sequence/2').

test(
  's_degree_sequence(+,+) is semidet. TRUE',
  [forall(s_degree_sequence_test(G,DegreeSeq,true))]
):-
  s_degree_sequence(G, DegreeSeq).
test(
  's_degree_sequence(+,+) is semidet. FALSE',
  [fail,forall(s_degree_sequence_test(G,DegreeSeq,fail))]
):-
  s_degree_sequence(G, DegreeSeq).

s_degree_sequence_test(G, [4,3,2,2,1], true):-
  s_test_graph(various(1), G).
s_degree_sequence_test(G, DegreeSeq2, fail):-
  s_degree_sequence_test(G, DegreeSeq1, true),
  permutation(DegreeSeq2, DegreeSeq1),
  DegreeSeq1 \= DegreeSeq2.

:- end_tests('s_degree_sequence/2').



%! s_indegree(+Graph:ugraph, +V, +InDegree:nonneg) is semidet.
%! s_indegree(+Graph:ugraph, +V, -InDegree:nonneg) is det.
%! s_indegree(+Graph:ugraph, -V, +InDegree:nonneg) is nondet.
%! s_indegree(+Graph:ugraph, -V, -InDegree:nonneg) is nondet.

s_indegree(G, V, N):- s_vertex(G, V), aggregate_all(count, s_edge(G, _-V), N).



%! s_max_degree(+Graph:ugraph, +MaxDegree:nonneg) is semidet.
%! s_max_degree(+Graph:ugraph, -MaxDegree:nonneg) is det.

s_max_degree(G, MaxDegree):-
  aggregate_all(max(Degree), s_degree(G, _, Degree), MaxDegree).

:- begin_tests('s_max_degree/2').

test(
  's_max_degree(+,+) is semidet. FALSE',
  [fail,forall(s_max_degree_test(GName,MaxDegree,false))]
):-
  s_test_graph(GName, G),
  s_max_degree(G, MaxDegree).
test(
  's_max_degree(+,+) is semidet. TRUE',
  [forall(s_max_degree_test(GName,MaxDegree,true))]
):-
  s_test_graph(GName, G),
  s_max_degree(G, MaxDegree).

s_max_degree_test(equiv(1), 3, true).
s_max_degree_test(GName, Degree2, fail):-
  s_max_degree_test(GName, Degree1, true),
  between(0, Degree1, Degree2),
  Degree1 =\= Degree2.

:- end_tests('s_max_degree/2').



%! s_min_degree(+Graph:ugraph, +MinDegree:nonneg) is semidet.
%! s_min_degree(+Graph:ugraph, -MinDegree:nonneg) is det.

s_min_degree(G, MinDegree):-
  aggregate_all(min(Degree), s_degree(G, _, Degree), MinDegree).

:- begin_tests('s_min_degree/2').

test(
  's_min_degree(+,+) is semidet. TRUE',
  [forall(s_min_degree_test(GName,MinDegree,true))]
):-
  s_test_graph(GName, G),
  s_min_degree(G, MinDegree).

s_min_degree_test(various(2), 1, true).

:- end_tests('s_min_degree/2').



%! s_order(+Graph:ugraph, +Order:nonneg) is semidet.
%! s_order(+Graph:ugraph, -Order:nonneg) is det.
% The *order* of a graph is the cardinality of its vertices.

s_order(G, Order):-
  s_vertices(G, Vs),
  length(Vs, Order).



%! s_outdegree(+Graph:ugraph, +V, +InDegree:nonneg) is semidet.
%! s_outdegree(+Graph:ugraph, +V, -InDegree:nonneg) is det.
%! s_outdegree(+Graph:ugraph, -V, +InDegree:nonneg) is nondet.
%! s_outdegree(+Graph:ugraph, -V, -InDegree:nonneg) is nondet.

s_outdegree(G, V, N):- s_vertex(G, V), aggregate_all(count, s_edge(G, _-V), N).



%! s_size(+Graph:ugraph, +Size:nonneg) is semidet.
%! s_size(+Graph:ugraph, -Size:nonneg) is det.
% The *size* of a graph is the cardinality of its edges.

s_size(G, Size):-
  s_edges(G, Es),
  length(Es, Size).
