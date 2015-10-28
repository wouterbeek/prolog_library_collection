:- module(
  s_metrics,
  [
    s_connected_component/2, % +Graph:ugraph
                             % +ConnectedComponent:ugraph
    s_degree_sequence/2, % +Graph:ugraph
                         % -DegreeSequence:list(nonneg)
    s_graph_degree/2, % +Graph:ugraph
                      % -Degree:nonneg
    s_graph_order/2, % +Graph:ugraph
                     % ?Order:nonneg
    s_graph_size/2, % +Graph:ugraph
                    % ?Size:nonneg
    s_maximum_degree/2, % +Graph:ugraph
                        % ?Degree:nonneg
    s_minimum_degree/2 % +Graph:ugraph
                       % ?Degree:nonneg
  ]
).

/** <module> Graph theory: Metrics

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(graph/s/s_subgraph)).
:- use_module(library(graph/s/s_type)).
:- use_module(library(graph/s/s_vertex)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(plunit)).





%! s_connected_component(+Graph:ugraph, +ConnectedComponent:ugraph) is semidet.
%! s_connected_component(+Graph:ugraph, -ConnectedComponent:ugraph) is nondet.

s_connected_component(G, Comp):-
  s_subgraph(Comp, G),
  s_connected_graph(Comp).

/* Alternative implementation:
s_connected_component(CC, Graph):-
  s_graph_components(Graph, Vs0, Es0),
  replace_graph_components(Vs0, Es0),
  repeat,
  (graph([V|Vs], Es) -> true ; !, fail),
  s_connected_component(Vs, SolVs, Es, SolEs, [V], CC),
  replace_graph_components(SolVs, SolEs).

s_connected_component(Vs1, SolVs, Es1, SolEs, [H1|T], CC2):-
  % @tbd Use the fact that `Es1` is sorted.
  select(H1-H2, Es1, Es2), !,
  ord_del_element(Vs1, H2, Vs2),
  s_connected_component(Vs2, SolVs, Es2, SolEs, [H2,H1|T], CC1),
  ord_add_element(CC1, H2, CC2).
s_connected_component(Vs, Vs, Es, Es, [H], [H]):- !.
s_connected_component(Vs, SolVs, Es, SolEs, [_|T], CC):-
  s_connected_component(Vs, SolVs, Es, SolEs, T, CC).

replace_graph_components(Vs, Es):-
  retractall(graph(_,_)),
  assert(graph(Vs,Es)).
*/



%! s_degree_sequence(+Graph:ugraph, +DegreeSequence:list(nonneg)) is semidet.
%! s_degree_sequence(+Graph:ugraph, -DegreeSequence:list(nonneg)) is det.

s_degree_sequence(G, DegreeSeq):-
  s_vertices(G, Vs),
  maplist(\V^Degree^s_vertex_degree(G, V, Degree), Vs, VDegrees),
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
  s_graph_test(various(1), G).
s_degree_sequence_test(G, DegreeSeq2, fail):-
  s_degree_sequence_test(G, DegreeSeq1, true),
  permutation(DegreeSeq2, DegreeSeq1),
  DegreeSeq1 \= DegreeSeq2.

:- end_tests('s_degree_sequence/2').



%! s_graph_degree(+Graph:ugraph, +Degree:nonneg) is semidet.
%! s_graph_degree(+Graph:ugraph, -Degree:nonneg) is semidet.
% The *degree* of a graph is the degree of each of its vertices.
%
% This fails silently for non-regular graphs (which do not have a degree).

s_graph_degree(G, Degree):-
  (   s_vertex(G, V1),
      s_vertex_degree(G, V1, Degree)
  ->  forall(s_vertex(G, V), s_vertex_degree(G, V, Degree))
  ;   Degree = 0
  ).



%! s_graph_order(+Graph:ugraph, +Order:nonneg) is semidet.
%! s_graph_order(+Graph:ugraph, -Order:nonneg) is det.
% The *order* of a graph is the cardinality of its vertices.

s_graph_order(G, Order):-
  s_vertices(G, Vs),
  length(Vs, Order).



%! s_graph_size(+Graph:ugraph, +Size:nonneg) is semidet.
%! s_graph_size(+Graph:ugraph, -Size:nonneg) is det.
% The *size* of a graph is the cardinality of its edges.

s_graph_size(G, Size):-
  s_edges(G, Es),
  length(Es, Size).



%! s_maximum_degree(+Graph:ugraph, +MaxDegree:nonneg) is semidet.
%! s_maximum_degree(+Graph:ugraph, -MaxDegree:nonneg) is det.

s_maximum_degree(G, MaxDegree):-
  aggregate_all(max(Degree), s_vertex_degree(G, _, Degree), MaxDegree).

:- begin_tests('s_maximum_degree/2').

test(
  's_maximum_degree(+,+) is semidet. FALSE',
  [fail,forall(s_maximum_degree_test(GName,MaxDegree,false))]
):-
  s_graph_test(GName, G),
  s_maximum_degree(G, MaxDegree).
test(
  's_maximum_degree(+,+) is semidet. TRUE',
  [forall(s_maximum_degree_test(GName,MaxDegree,true))]
):-
  s_graph_test(GName, G),
  s_maximum_degree(G, MaxDegree).

s_maximum_degree_test(equiv(1), 3, true).
s_maximum_degree_test(GName, Degree2, fail):-
  s_maximum_degree_test(GName, Degree1, true),
  between(0, Degree1, Degree2),
  Degree1 =\= Degree2.

:- end_tests('s_maximum_degree/2').



%! s_minimum_degree(+Graph:ugraph, +MinDegree:nonneg) is semidet.
%! s_minimum_degree(+Graph:ugraph, -MinDegree:nonneg) is det.

s_minimum_degree(G, MinDegree):-
  aggregate_all(min(Degree), s_vertex_degree(G, _, Degree), MinDegree).

:- begin_tests('s_minimum_degree/2').

test(
  's_minimum_degree(+,+) is semidet. TRUE',
  [forall(s_minimum_degree_test(GName,MinDegree,true))]
):-
  s_graph_test(GName, G),
  s_minimum_degree(G, MinDegree).

s_minimum_degree_test(various(2), 1, true).

:- end_tests('s_minimum_degree/2').
