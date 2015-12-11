:- module(
  s_subgraph,
  [
    s_connected_component/2, % +Graph:ugraph
                             % -ConnectedComponent:ugraph
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
@version 2015/10, 2015/12
*/

:- use_module(library(graph/s/s_graph)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(set/set_ext)).





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
