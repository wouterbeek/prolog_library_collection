:- module(
  ugraphs_ext,
  [
    bipartite/3, % +Graph:ugraph
                 % -S1:list(vertex)
                 % -S2:list(vertex)
    complete/1, % +Graph:ugraph
    complete/2, % +VG:list(vertex)
                % ?Graph:ugraph
    component/2, % +C:ugraph
                 % +Graph:ugraph
    edge_induced_subgraph/3, % +Graph:ugraph
                             % +ESubG:list(edge)
                             % -SubG:ugraph
    empty/1, % ?Graph:ugraph
    harary/3, % +K:integer
              % +N:integer
              % -H:ugraph
    line_graph/2, % +Graph:ugraph
                  % -LineG:ugraph
    ugraph/1, % +Graph:ugraph
    ugraph_direct_subgraph/2, % ?SubGraph:ugraph
                              % +Graph:ugraph
    ugraph_edge/2, % +Options:list(nvpair)
                   % ?Edge:edge
    ugraph_maximum_components/2, % +Graph:ugraph
                                 % -MaximumComponent:ugraph
    ugraph_neighbor/3, % +Options:list(nvpair)
                       % ?Vertex:vertex
                       % ?Neighbor:vertex
    ugraph_subgraph/2, % ?G1:ugraph
                       % +G2:ugraph
    ugraph_vertex/2, % +Options:list(nvpair)
                     % ?Vertex:vertex
    vertex_induced_subgraph/3 % +Graph:ugraph
                              % +VSubG:list(vertex)
                              % -SubG:ugraph
  ]
).

/** <module> UGRAPH extensions

Methods that extend the SWI-Prolog builtin library for undirected graphs.

This uses the SWI-Prolog library =ugraph=, originally
written by Richard O'Keefe. Also implemented in YAP and
SICStus Prolog.

Searching for a pair takes $2|V(G)|$ instead of $|V(G)|^2$.

All edges $\tuple{v, w}$ occur twice in a ugraph,
i.e. $v-[\ldots, w, \ldots]$ and $w-[\ldots, v, \ldots]$.

---+ Types

---++ 2D vertex

A 2D vertex (or =|vertice_coorinate|=) is a compound term representing
a 2D representation of a vertex.
It has the form =|vertex_coordinate(<vertive>, <2d_coordinate>)|=.

@author Wouter Beek
@version 2012/09-2013/02
*/

:- use_module(generic(list_ext)).
:- use_module(generic(meta_ext), except([complete/3])).
:- use_module(graph_theory(graph_generic)).
:- use_module(math(math_ext)).

:- meta_predicate harary(+,2,+,-).



bipartite(Graph, S1, S2):-
  ugraphs:edges(Graph, EG),
  bipartite0(EG, [], S1, [], S2).

bipartite0([], S1, S1, S2, S2).
bipartite0([V-W | EG], H_S1, S1, H_S2, S2):-
  % For unordered graphs we only need to consider each edge in one direction.
  V > W,
  !,
  bipartite0(EG, H_S1, S1, H_S2, S2).
bipartite0([V-W | EG], H_S1, S1, H_S2, S2):-
  \+(member(W, H_S1)),
  \+(member(V, H_S2)),
  ord_add_element(H_S1, V, New_H_S1),
  ord_add_element(H_S2, W, New_H_S2),
  bipartite0(EG, New_H_S1, S1, New_H_S2, S2).
% Fit the edge either way.
bipartite0([W-V | EG], H_S1, S1, H_S2, S2):-
  \+(member(W, H_S1)),
  \+(member(V, H_S2)),
  ord_add_element(H_S1, V, New_H_S1),
  ord_add_element(H_S2, W, New_H_S2),
  bipartite0(EG, New_H_S1, S1, New_H_S2, S2).

%% complete(+Graph:ugraph) is semidet.
% Succeeds for complete graphs.
%
% @see complete/2

complete(Graph):-
  ugraphs:vertices(Graph, VG),
  complete(VG, Graph).

%% complete(+VG:list(vertex), +Graph:ugraph) is semidet.
%% complete(+VG:list(vertex), -Graph:ugraph) is det.
% Succeeds if the given graph is complete, or generates the complete graph
% from the given vertices.
%
% *Definition*: A complete graph is one in which all different vertices
%               are connected.
%
% @param VG An ordered set of vertices.
% @param Graph A ugraph, i.e., a list of S-expressions.

complete(VG, Graph):-
  complete(VG, VG, Graph).

complete(_VG, [], []).
complete(VG, [V | Vs], [V-Ws | Graph]):-
  ord_del_element(VG, V, Ws),
  complete(VG, Vs, Graph).

%% component(+C:ugraph, +Graph:ugraph) is semidet.
% Succeeds of the former graph is a component of the latter.
%
% *Definition*: A component is a maximal connected subgraph.

component(C, G):-
  subgraph([graph(G)], C),
  connected([graph(C)]),
  \+((
    subgraph([graph(G)], D),
    strict_subgraph([graph(D)], C),
    connected([graph(D)])
  )).

ugraph_maximum_components(Graph, MaxComps):-
  ugraph_maximum_components0([Graph], MaxComps).

ugraph_maximum_components0([], []).
ugraph_maximum_components0([H | T], [H | Sol]):-
  connected(H),
  !,
  ugraph_maximum_components0(T, Sol).
ugraph_maximum_components0([H | T], Sol):-
  findall(
    DSG,
    ugraph_direct_subgraph(DSG, H),
    DSGs
  ),
  append(T, DSGs, NewT),
  ugraph_maximum_components0(NewT, Sol).

ugraph_direct_subgraph(DirectSubGraph, Graph):-
  ugraphs:vertices(Graph, Vertices),
  ugraphs:edges(Graph, Edges1),
  nth0(_I, Edges1, V-W, Edges2),
  V > W,
  nth0(_J, Edges2, W-V, Edges3),
  vertices_edges_to_ugraph(Vertices, Edges3, DirectSubGraph).

%% ugraph_edge(+Options:list(nvpair), ?Edge:edge) is nondet.
% Edges in a UGRAPH.
%
% @param Options A list of the following name-value pairs:
%        1. =directed(DirectedGraph:boolean)= Whether or not the
%           directionality of the edge is taken into account.
%        2. =graph(Graph:atom)= The atomic name of the graph to which =Edge=
%           must belong.
% @param Edge An edge, of the form =|From-To|=.

ugraph_edge(Options, From-To):-
  option(graph(Graph), Options),
  ugraphs:edges(Graph, Edges),

  % Whether the edge's directionality is relevant or not.
  %option(directed(DirectedGraph), Options, false),
  %(
  %  call(DirectedGraph)
  %->
    member(From-To, Edges).
  %;
  %  (
  %    member(From-To, Edges)
  %  ;
  %    member(To-From, Edges)
  %  )
  %).

%% edge_induced_subgraph(
%%   +Graph:ugraph,
%%   +ESubG:list(edge),
%%  -SubG:ugraph
%% ) is det.
% Returns the edge-induced subgraph.

edge_induced_subgraph(Graph, ESubG, SubG):-
  ugraphs:edges(Graph, Es),
  ord_subtract(Es, ESubG, DelEs),
  del_edges(Graph, DelEs, SubG).

%% ugraph_edges(+Options:list(nvpair), -Edges:ord_set(edge)) is det.

ugraph_edges(Options, Edges):-
  option(graph(Graph), Options),
  % Use the swipl builtin.
  ugraphs:edges(Graph, Edges).

%% empty(+Graph:ugraph) is semidet.
%% empty(-Graph:ugraph) is det.
% Succeeds on the empty graph or returns the empty graph.

empty([]).

%% harary(+K:integer, +N:integer, -H:ugraph) is det.
% Generates a Harary graph that is K-connected and that has N vertices.
%
% *Definition*: A Harary graph is a K-connected simple graph with
%               N vertices and the minimal number of edges.
%
% @param K The connectedness of the Harary graph.
% @param N The number of vertices.
% @param H An undirected Harary graph.

harary(K, N, H):-
  even(K),
  !,
  V_Last is N - 1,
  numlist(0, V_Last, Vs),
  Half_K is K / 2,
  findall(
    V-Neighbors,
    (
      member(V, Vs),
      Min is (V - Half_K) mod N,
      Max is (V + Half_K) mod N,
      cyclic_numlist(Min, Max, N, Ns_),
      select(V, Ns_, Neighbors)
    ),
    H
  ).
harary(K, N, H):-
  even(N),
  !,
  NewK is K - 1,
  harary(NewK, N, Graph),
  harary(Graph, id, N, H).
harary(K, N, H):-
  NewK is K - 1,
  harary(NewK, N, Graph),
  harary(Graph, pred, N, H).

harary(Graph, Mod:P, N, H):-
  Call =.. [P, N, NewN],
  call(Mod:Call),
  findall(
    V-Neighbors,
    (
      member(V-Ms, Graph),
      W is V + (NewN / 2),
      (
        W =< N
      ->
        Neighbors = [V | Ms]
      ;
        Neighbors = Ms
      )
    ),
    H
  ).

is_ugraph_edge(V-Ws):-
  atomic(V),
  is_list(Ws).

%% line_graph(+Graph:ugraph, -LineG:ugraph) is det.
% Returns the line graph for the given graph.
%
% *Definition*: The line graph G' of graph G has V(G') = E(G) and
%               $E(G') = \setoff{\tuple{\tuple{x, y},\tuple{y, z}} \vert
%               \tuple{x, y}, \tuple{y, z} \in E(G)}$.
%
% *Representation*: Vertex $V \in V(LineG)$ that is based on edge
%                   $\tuple{X, Y} \in E(G)$ is represented in the following
%                   way: =|V = X/Y|=, where $X < Y$.
%
% *Restriction*: Line graphs can only be created for undirected graphs,
%                see undirected/1.
%
% *Restriction*: Line graphs can only consist of vertices that adhere to
%                the sorting relation <.
%
% @tbd Allow a comparator argument, so that vertices that do not compare
%      with < are allowed as well.

line_graph(Graph, LineG):-
  ugraphs:edges(Graph, EG),
  findall(
    V/W-Neighbors,
    (
      member(V-W, EG),
      V < W,
      findall(
        X/Y,
        (
          member(X-Y, EG),
          X < Y,
          (
            X == V
          ;
            X == W
          ;
            Y == V
          ;
            Y == W
          ),
          X/Y \== V/W
        ),
        Neighbors
      )
    ),
    LineG
  ).

%% ugraph(+Graph:ugraph) is semidet.
% Succeeds if the given graph could be undirected.
%
% An undirected graph is represented as a ugraph that has a symmerical
% closure over its edges.
%
% Every undirected graph succeeds for this predicate, but not every graph
% that succeeds for this predicate is undirected. This depends on the
% intention of the programmer, since a directed graph may have symmetric
% closure of its edges as well.

ugraph(Graph):-
  ugraphs:edges(Graph, Edges),
  forall(
    member(V-W, Edges),
    member(W-V, Edges)
  ).

%% ugraph_neighbor(
%%   +Options:list(nvpair),
%%   ?Vertex:vertex,
%%   ?Neighbor:vertex
%% ) is nondet.
% Neighboring vertex.
%
% @param Options A list of the following name-value pairs:
%        1. =graph(Graph:ugraph)=

ugraph_neighbor(Options0, Vertex, Neighbor):-
  merge_options([directed(true)], Options0, Options),
  ugraph_edge(Options, Vertex-Neighbor).

%% ugraph_subgraph(+G1:ugraph, +G2:ugraph) is semidet.
%% ugraph_subgraph(-G1:ugraph, +G2:ugraph) is nondet.
% G1 is a subgraph of G2.

ugraph_subgraph(G1, G2):-
  ugraph_subgraph0(G1, G2, [], []).

ugraph_subgraph0([], [], _In, _Out).
ugraph_subgraph0([V-V1s | G1], [V-V2s | G2], In, Out):-
  % Predicate used from LIST_EXT.
  sublist(V1s_, V2s),
  ord_subtract(V1s_, Out, V1s),
  ord_union(In, V1s, NewIn),
  ugraph_subgraph0(G1, G2, NewIn, Out).
ugraph_subgraph0(G1, [V-_Vs | G2], In, Out):-
  \+ member(V, In),
  ord_add_element(Out, V, NewOut),
  ugraph_subgraph0(G1, G2, In, NewOut).

%% ugraph_vertex(+Options:list(nvpair), ?Vertex:vertex) is nondet.
% Vertices in a graph.
%
% @param Options A list of name-value pairs.
%        1. =graph(Graph:ugraph)= A UGRAPH.
% @param Vertex A vertex in =Graph=.

ugraph_vertex(Options, Vertex):-
  option(graph(Graph), Options),
  ugraphs:vertices(Graph, Vertices),
  member(Vertex, Vertices).

%% unsymmetric_edges(+Edges:list(edge), -UnsymmetricEdges:list(edge)) is det.
% Returns the unsymmetric edges for the given edges.
% For every pair of symmetric edges $\set{\tuple{V, W}, \tuple{W, V}}$
% we only take the edge for which the first member is smaller than the
% latter.
%
% *|Special case|*: Reflexive edges are symmetric and therefore removed
%                   entirely.

unsymmetric_edges(Edges, UnsymmetricEdges):-
  findall(
    V-W,
    (
      member(V-W, Edges),
      V < W
    ),
    UnsymmetricEdges
  ).

%% vertex_induced_subgraph(
%%   +Graph:ugraph,
%%   +VSubG:list(vertex),
%%   -SubG:ugraph
%% ) is det.
% Returns the vertex-induced subgraph.

vertex_induced_subgraph(Graph, VSubG, SubG):-
  ugraphs:vertices(Graph, Vs),
  ord_subtract(Vs, VSubG, DelVs),
  del_vertices(Graph, DelVs, SubG).

