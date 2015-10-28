:- module(
  s_graph,
  [
    s_edge/2, % +Graph:ugraph
              % ?Edge:pair
    s_edge_head/2, % +Edge:pair
                   % ?Head
    s_edge_string/2, % +Edge:pair
                     % -String:string
    s_edge_tail/2, % +Edge:pair
                   % ?Tail
    s_edges_vertices/2, % +Edges:list(pair)
                        % -Vertices:ordset
    s_graph_components/3 % ?Graph:ugraph
                         % ?Vertices:ordset
                         % ?Edges:ordset(pair)
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
    edges/2 as s_edges, % +Graph:ugraph
                        % -Edges:ordset(pair)
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
    ugraph_union/3 as s_graph_union, % +Graph1:ugraph
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
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_arrow)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl_term)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(ordsets)).
:- use_module(library(ugraphs), [vertices_edges_to_ugraph/3]).

:- multifile(error:has_type/2).
error:has_type(ugraph, X):-
  error:has_type(list(pair(any,list)), X).





%! s_edge(+Graph:ugraph, +Edge:pair) is semidet.
%! s_edge(+Graph:ugraph, -Edge:pair) is nondet.
% @throws instantiation_error if Graph is uninstantiated.

s_edge(G, _):-
  var(G), !,
  instantiation_error(G).
s_edge(G, E):-
  maplist(ground, [G,E]), !,
  s_edge0(G, E), !.
s_edge(G, E):-
  s_edge0(G, E).

s_edge0(G, Tail-Head):-
  member(Tail-Heads, G),
  member(Head, Heads).


%! s_edge_head(+Edge:pair, +Head) is semidet.
%! s_edge_head(+Edge:pair, -Head) is det.
% The head of a directed edge is the vertex the edge is "pointing to".

s_edge_head(_-Head, Head).



%! s_edge_string(+Edge:pair, -String:string)// is det.

s_edge_string(E, String):-
  string_phrase(s_edge_string(E), String).

s_edge_string(V-W) -->
  pl_term(V),
  " ",
  arrow(both, 3),
  " ",
  pl_term(W).



%! s_edge_tail(+Edge:pair, +Tail) is semidet.
%! s_edge_tail(+Edge:pair, -Tail) is det.
% The tail of a directed edge is the vertex the edge is "pointing from".

s_edge_tail(Tail-_, Tail).



%! s_edges_vertices(+Edges:list(pair), -Vertices:ordset) is det.
% Returns the vertices that occur in the given edges.

s_edges_vertices(Es, Vs):-
  pairs_keys_values(Es, Vs1a, Vs2a),
  list_to_ord_set(Vs1a, Vs1b),
  list_to_ord_set(Vs2a, Vs2b),
  ord_union(Vs1b, Vs2b, Vs).



%! s_edge(+Graph:ugraph, +Tail, +Head) is semidet.
%! s_edge(+Graph:ugraph, +Tail, -Head) is nondet.
%! s_edge(+Graph:ugraph, -Tail, +Head) is nondet.
%! s_edge(+Graph:ugraph, -Tail, -Head) is nondet.

s_edge(G, Tail, Head):-
  s_edge(G, Tail-Head).


%! s_edge(+Graph:ugraph, +Edge:pair, +Tail, +Head) is semidet.
%! s_edge(+Graph:ugraph, -Edge:pair, +Tail, -Head) is nondet.
%! s_edge(+Graph:ugraph, -Edge:pair, -Tail, +Head) is nondet.
%! s_edge(+Graph:ugraph, -Edge:pair, -Tail, -Head) is nondet.

s_edge(G, Tail-Head, Tail, Head):-
  s_edge(G, Tail-Head).



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
  nonvar(G), !,
  vertices(G, Vs),
  edges(G, Es).
s_graph_components(G, Vs, Es):-
  once(vertices_edges_to_ugraph(Vs, Es, G)).
