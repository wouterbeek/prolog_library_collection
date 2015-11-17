:- module(
  s_type,
  [
    s_bipartite/3, % +Graph:ugraph
                   % +Vertices1:ordset
                   % +Vertices2:ordset
    s_complete_graph/1, % +Graph:ugraph
    s_connected_graph/1, % +Graph:ugraph
    s_cubic_graph/1, % +Graph:ugraph
    s_cyclic_graph/1, % +Graph:ugraph
    s_cyclic_graph/2, % +Graph:ugraph
                      % ?Order:nonneg
    s_directed_graph/1, % +Graph:ugraph
    s_empty_graph/1, % ?Graph:ugraph
    s_graphic_graph/1, % +Graph:ugraph
    s_harary/3, % +K:nonneg
                % +N:nonneg
                % +Harary:ugraph
    s_line_graph/2, % +Graph:ugraph
                    % -LineGraph:ugraph
    s_regular_graph/1, % +Graph:ugraph
    s_regular_graph/2, % +Graph:ugraph
                       % -K:nonneg
    s_simple_graph/1, % +Graph:ugraph
    s_star_graph/1, % +Graph:ugraph
    s_star_graph/2, % +Graph:ugraph
                    % ?Order:nonneg
    s_strict_graph/1, % +Graph:ugraph
    s_strongly_connected_graph/1, % +Graph:ugraph
    s_underlying_graph/2, % +Graph:ugraph
                          % -Underlying:ugraph
    s_undirected_graph/1, % +Graph:ugraph
    s_weakly_connected_graph/1 % +Graph:ugraph
  ]
).

/** <module> Graph Types

Predicates for distinguishing different types/kinds of graphs.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(graph/graph_traverse)).
:- use_module(library(graph/graph_walk)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(graph/s/s_metrics)).
:- use_module(library(graph/s/s_vertex)).
:- use_module(library(list_ext)).
:- use_module(library(math/math_ext)).
:- use_module(library(ordsets)).





%! s_bipartite(+Graph:ugraph, +Vertices:ordset, +Vertices2:ordset) is semidet.
%! s_bipartite(+Graph:ugraph, -Vertices:ordset, -Vertices2:ordset) is nondet.

s_bipartite(G, Vs1, Vs2):-
  s_edges(G, Es),
  s_bipartite(Es, [], Vs1, [], Vs2).

s_bipartite([], Vs1, Vs1, Vs2, Vs2).
% Inconsistent 1: Both belong to set 1.
s_bipartite([V-W|_], L1, _, _, _):-
  member(V, W, L1), !,
  fail.
% Inconsistent 2: Both belong to set 2.
s_bipartite([V-W|_], _, _, L2, _):-
  member(V, W, L2), !,
  fail.
% V was already fitted: fit W accordingly.
s_bipartite([V-W|Es], L1, Vs1, L2, Vs2):-
  member(V, L1), !,
  ord_add_element(L2, W, NewL2),
  s_bipartite(Es, L1, Vs1, NewL2, Vs2).
% W was already fitted: fit V accordingly.
s_bipartite([V-W|Es], L1, Vs1, L2, Vs2):-
  member(W, L2), !,
  ord_add_element(L1, V, NewL1),
  s_bipartite(Es, NewL1, Vs1, L2, Vs2).
% Both V and W have to be fitted.
% This can be done in two ways.
s_bipartite([V-W|Es], L1, Vs1, L2, Vs2):-
  ord_add_element(L1, V, NewL1),
  ord_add_element(L2, W, NewL2),
  s_bipartite(Es, NewL1, Vs1, NewL2, Vs2).
s_bipartite([V-W|Es], L1, Vs1, L2, Vs2):-
  ord_add_element(L2, V, NewL2),
  ord_add_element(L1, W, NewL1),
  s_bipartite(Es, NewL1, Vs1, NewL2, Vs2).



%! s_complete_graph(+Graph:ugraph) is semidet.
% Complete graphs and connected graphs are the same thing.

s_complete_graph(G):-
  s_connected_graph(G).



%! s_connected_graph(+Graph:ugraph) is semidet.
% Succeeds if the given graph is connected.
%
% *Definition*: A graph is **connected** if all pairs of vertices are connected.
%
% *Definition*: Two vertices are connected if there is a path between them.
% So vertice connectedness is just path existence.

s_connected_graph(G):-
  s_vertices(G, Vs),
  forall(
    member(V, W, Vs),
    % A path connects vertices V and W.
    % Optimization: Do not traverse vertices more than once.
    traverse(s_edges, s_neighbor, G, V, W, [distinct_vertices(true)])
  ).



%! s_cubic_graph(+Graph:ugraph) is semidet.
% Succeeds if the given graph is cubic.
%
% *Definition*: A cubic graph is a 3-regular graph.

s_cubic_graph(G):-
  s_regular_graph(G, 3).



%! s_cyclic_graph(+Graph:ugraph) is semidet.

s_cyclic_graph(G):-
  s_cyclic_graph(G, _).
/* Alternative implementation:
s_cyclic_graph(G):-
  once(
    traverse(
      G,
      s_vertices,
      s_neighbor,
      V,
      V,
      [closed(true),unique_edge(true),unique_vertex(true)]
    )
  ).
*/

%! s_cyclic_graph(+Graph:ugraph, +Order:nonneg) is semidet.
%! s_cyclic_graph(+Graph:ugraph, -Order:nonneg) is semidet.
% A *cyclic* graph is a graph for which every vertex has the same degree.
% The number of vertices is called the order of the cyclic graph.

s_cyclic_graph(G, Order):-
  s_degree_sequence(G, [H|T]),
  repeating_list(H, Order, [H|T]).

:- begin_tests('s_cyclic_graph/2').

test(
  's_cyclic_graph(+,-) is semidet. TRUE',
  [forall(s_cyclic_graph_test(GName,Order,true))]
):-
  s_graph_test(GName, G),
  s_cyclic_graph(G, Order).

s_cyclic_graph_test(cycle(3), 3, true).
s_cyclic_graph_test(cycle(1), 4, true).

:- end_tests('s_cyclic_graph/2').



%! s_directed_graph(+Graph:ugraph) is semidet.
% Succeeds for some directed graphs.
%
% Every directed graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is directed. This depends on the
% intention of the programmer, since an directed graph may have symmetric
% closure of its edges merely by chance.

s_directed_graph(G):-
  s_edge(G, V-W),
  \+ s_edge(G, W-V).



%! s_empty_graph(+Graph:ugraph) is semidet.
%! s_empty_graph(-G) is det.

s_empty_graph([]).



%! s_graphic_graph(+Graph:ugraph) is semidet.
% Succeeds if the given degree sequence represents a simple graph.
%
% *Definition*: A sequence is s_graphic_graph if the integers correspond
%               to the degrees of a simple graph.
%
% Havel-Hakimi theorem: Consider a list $s = [d_1, \ldots, d_n]$ of $n$
% numbers in descending order. This list is **graphic** iff
% $s^* = [d_1^*, \ldots, d_n^*]$ of $n - 1$ numbers is graphic as well, where
% $d_i^* = d_{i + 1} - 1, \text{for} i = 1, \ldots, d_1
%          d_{i + 1}, \text{otherwise}$

s_graphic_graph(G):-
  s_degree_sequence(G, Seq),
  graphic_sequence(Seq).

%! graphic_sequence(+Sequence:list(nonneg)) is semidet.
% Succeeds is Sequence is a graphic sequence.

graphic_sequence(Seq):-
  inflist(0, Seq), !.
graphic_sequence([H | T]):-
  length(T, LT),
  H =< LT,
  length_cut(T, H, T1, T2),
  maplist(succ, NewT1, T1),
  append(NewT1, T2, NewT_),
  sort(1, @>=, NewT_, NewT),
  graphic_sequence(NewT).



%! s_harary(+K:nonneg, +N:nonneg, +Harary:ugraph) is semidet.
%! s_harary(+K:nonneg, +N:nonneg, -Harary:ugraph) is det.
% Generates a Harary graph that is K-connected and that has N vertices.
%
% *Definition*: A Harary graph is a K-connected simple graph with
%               N vertices and the minimal number of edges.
%
% @arg K The connectedness of the Harary graph.
% @arg N The number of vertices.
% @arg Harary An undirected Harary graph.

s_harary(K, N, H):-
  is_even(K), !,
  VLast is N - 1,
  numlist(0, VLast, Vs),
  HalfK is K / 2,
  findall(
    V-Ns,
    (
      member(V, Vs),
      Min is (V - HalfK) mod N,
      Max is (V + HalfK) mod N,
      cyclic_numlist(Min, Max, N, Ns0),
      select(V, Ns0, Ns)
    ),
    H
  ).
s_harary(K, N, H):-
  is_even(N), !,
  NewK is K - 1,
  s_harary(NewK, N, G),
  s_harary(G, =, N, H).
s_harary(K, N, H):-
  NewK is K - 1,
  s_harary(NewK, N, G),
  s_harary(G, pred, N, H).

s_harary(G, P, N, H):-
  call(P, N, NewN),
  findall(
    V-Ns,
    (
      member(V-Ms, G),
      W is V + (NewN / 2),
      (   W =< N
      ->  Ns = [V|Ms]
      ;   Ns = Ms
      )
    ),
    H
  ).

%! cyclic_numlist(
%!   +Low:integer,
%!   +High:integer,
%!   +CycleLength:integer,
%!   -NumList:list(integer)
%! ) is det.
% Generates a cyclic numlist.
% This method works on a off-by-zero basis.
% We return the numbers in a sorted order.

cyclic_numlist(Low, High, _CycleLength, NumList):-
  Low < High, !,
  numlist(Low, High, NumList).
cyclic_numlist(Low, High, CycleLength, NumList):-
  Top is CycleLength - 1,
  numlist(Low, Top, HigherNumList),
  numlist(0, High, LowerNumList),
  append(LowerNumList, HigherNumList, NumList).



%! s_line_graph(+Graph:ugraph, -LineGraph:ugraph) is det.
% Returns the line graph for the given graph.
%
% *Definition*: The line graph G' of graph G has V(G') = E(G) and
%               $E(G') = \  {\tuple{\tuple{x, y},\tuple{y, z}} \vert
%               \tuple{x, y}, \tuple{y, z} \in E(G)}$.
%
% *Representation*: Vertex $V \in V(LineG)$ that is based on edge
%                   $\tuple{X, Y} \in E(G)$ is represented in the following
%                   way: =|V = X/Y`, where $X < Y$.
%
% *Restriction*: Line graphs can only be created for undirected graphs,
%                see undirected/1.
%
% *Restriction*: Line graphs can only consist of vertices that adhere to
%                the sorting relation <.
%
% @tbd Allow a comparator argument, so that vertices that do not compare
%      with < are allowed as well.

s_line_graph(G, LineG):-
  s_edges(G, Es),
  findall(
    V/W-Ns,
    (
      member(V-W, Es),
      V @< W,
      findall(
        X/Y,
        (
          member(X-Y, Es),
          X < Y,
          (   X == V
          ;   X == W
          ;   Y == V
          ;   Y == W
          ),
          X/Y \== V/W
        ),
        Ns
      )
    ),
    LineG
  ).



%! s_regular_graph(+Graph:ugraph) is semidet.
% A *regular* graph is a graph with a (single) degree,
% i.e., every vertex has the same degree.

s_regular_graph(G):-
  s_regular_graph(G, _).


%! s_regular_graph(+Graph:ugraph, -K:nonneg) is semidet.
% Returns the degree of the given graph, if it is regular.

s_regular_graph(G, K):-
  s_graph_degree(G, K).

:- begin_tests('s_regular_graph/1').

test(
  's_regular_graph(+) is semidet. TRUE',
  [forall(s_graph_test(regular(_),G))]
):-
  s_regular_graph(G).

test(
  's_regular_graph(+) is semidet. FAIL',
  [fail,forall(s_graph_test(nonregular(_),G))]
):-
  s_regular_graph(G).

:- end_tests('s_regular_graph/1').



%! s_simple_graph(+Graph:ugraph) is semidet.
% *Definition*: A **simple graph** is an undirected graph
%               with no doubly occurring edges and no loops.
%
% @tbd We cannot represent doubly occurring/weighted edges/arcs yet.

s_simple_graph(G):-
  s_undirected_graph(G),
  \+ s_cyclic_graph(G).



%! s_star_graph(+Graph:ugraph) is semidet.

s_star_graph(G):-
  s_star_graph(G, _).

%! s_star_graph(+Graph:ugraph, +Order:nonneg) is semidet.
%! s_star_graph(+Graph:ugraph, -Order:nonneg) is semidet.

s_star_graph(G, Order):-
  s_graph_order(G, Order),
  s_degree_sequence(G, [Order|T]),
  inflist(1, [Order|T]).



%! s_strict_graph(+Graph:ugraph) is semidet.
% Succeeds on strict directed graphs.
%
% *Definition*: A strict directed graph has no loops
%               and no double occurring arcs.
%
% Strict graphs and simple graphs are the same thing.

s_strict_graph(G):-
  s_simple_graph(G).



%! s_strongly_connected_graph(+Graph:ugraph) is semidet.
% Succeeds if the given graph is strongly connected.
%
% *Definition*: A directed graph is **strongly connected** if
%               all pairs of vertices are connected via a directed path.

s_strongly_connected_graph(G):-
  s_vertices(G, Vs),
  forall(
    member(V, W, Vs),
    path(s_edges, s_neighbor, G, V, W)
  ).




%! s_underlying_graph(+Graph:ugraph, -UnderlyingGraph:ugraph) is det.
% Returns the underlying undirected graph for the given directed graph.
%
% The **underlying graph** of a directed graph *G*
% is the directed graph that is obtained by
% replacing all directed edges of *G* with undirected edges.

s_underlying_graph(G, UG):-
  s_graph_components(G, Vs, Es1),
  maplist(inv_pair0, Es1, Es2),
  append(Es1, Es2, Es3),
  list_to_set(Es3, Es4),
  s_graph_components(UG, Vs, Es4).

inv_pair0(X-Y, Y-X).



%! s_undirected_graph(+Graph:ugraph) is semidet.
% Succeeds if the given graph could be undirected.
%
% An undirected graph is represented as a ugraph that has a symmerical
% closure over its edges.
%
% Every undirected graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is undirected. This depends on the
% intention of the programmer, since a directed graph may have symmetric
% closure of its edges as well.

s_undirected_graph(G):-
  \+ s_directed_graph(G).



%! s_weakly_connected_graph(+Graph:ugraph) is semidet.
% Succeeds if the given digraph is weakly connected.
%
% *Definition*: A weakly connected digraph has a connected underlying graph.

s_weakly_connected_graph(G):-
  s_underlying_graph(G, UG),
  s_connected_graph(UG).
