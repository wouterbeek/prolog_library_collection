:- module(
  s_subgraph,
  [
    s_direct_subgraph/2, % ?SubGraph:ugraph
                         % +Graph:ugraph
    s_edge_induced_subgraph/3, % ?SubGraph:ugraph
                               % ?Edges:ordset(pair)
                               % +Graph:ugraph
    s_strict_subgraph/2, % ?SubGraph:ugraph
                         % +Graph:ugraph
    s_subgraph/2, % ?SubGraph:ugraph
                  % +Graph:ugraph
    s_vertex_induced_subgraph/3 % ?SubGraph:ugraph
                                % ?Vertices:ordset
                                % +Graph:ugraph
  ]
).

/** <module> Graph theory: Subgraphs

Subgraph operations.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(graph/s/s_graph)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(set/set_ext)).





%! s_direct_subgraph(+SubGraph:ugraph, +Graph:ugraph) is semidet.
%! s_direct_subgraph(-SubGraph:ugraph, +Graph:ugraph) is nondet.
% S' is a *direct subgraph* of S iff S' is a subgraph of S
% and every strict subgraph S'' of S is also a subgraph of S'.

s_direct_subgraph(SubG, G):-
  s_vertices(G, Vs),
  direct_subset(KeepVs, Vs),
  s_vertex_induced_subgraph(SubG, KeepVs, G).



%! s_edge_induced_subgraph(+SubG:ugraph, +KeepEs:ordset(pair), +G:ugraph) is semidet.
%! s_edge_induced_subgraph(+SubG:ugraph, -KeepEs:ordset(pair), +G:ugraph) is semidet.
%! s_edge_induced_subgraph(-SubG:ugraph, +KeepEs:ordset(pair), +G:ugraph) is nondet.
%! s_edge_induced_subgraph(-SubG:ugraph, -KeepEs:ordset(pair), +G:ugraph) is nondet.

s_edge_induced_subgraph(SubG, KeepEs, G):-
  s_edges(G, Es),
  (   var(KeepEs)
  ->  subset(KeepEs, Es)
  ;   true
  ),
  ord_subtract(Es, KeepEs, DelEs),
  s_del_edges(G, DelEs, SubG).



%! s_strict_subgraph(+StrictSubG:ugraph, +G:ugraph) is semidet.
%! s_strict_subgraph(-StrictSubG:ugraph, +G:ugraph) is nondet.
% S' is a *strict subgraph* of S iff S' is a subgraph of S and S' is not S.

s_strict_subgraph(StrictSubG, G):-
  s_subgraph(StrictSubG, G),
  StrictSubG \== G.



%! s_subgraph(+SubG:ugraph, +G:ugraph) is semidet.
%! s_subgraph(-SubG:ugraph, +G:ugraph) is nondet.

s_subgraph(SubG, G):-
  s_graph_components(G, Vs, Es),
  subset(SubEs, Es),
  s_graph_components(SubG, Vs, SubEs).



%! s_vertex_induced_subgraph(+SubG:ugraph, +Vs:ordset, +G:ugraph) is semidet.
%! s_vertex_induced_subgraph(+SubG:ugraph, -Vs:ordset, +G:ugraph) is semidet.
%! s_vertex_induced_subgraph(-SubG:ugraph, +Vs:ordset, +G:ugraph) is nondet.
%! s_vertex_induced_subgraph(-SubG:ugraph, -Vs:ordset, +G:ugraph) is nondet.

s_vertex_induced_subgraph(SubG, KeepVs, G):-
  s_vertices(G, Vs),
  (   var(KeepVs)
  ->  subset(KeepVs, Vs)
  ;   true
  ),
  ord_subtract(Vs, KeepVs, DelVs),
  s_del_vertices(G, DelVs, SubG).
