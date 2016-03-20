:- module(
  s_edge,
  [
    s_edge_endpoint/2, % +Edge:pair
                       % ?Endpoint
    s_edge_head/2, % +Edge:pair
                   % ?Head
    s_edge_string/2, % +Edge:pair
                     % -String:string
    s_edge_tail/2, % +Edge:pair
                   % ?Tail
    s_edges_vertices/2 % +Edges:ordset(pair)
                       % -Vertices:ordset
  ]
).

/** <module> Graph theory: Edges

@author Wouter Beek
@version 2015/10, 2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(ordsets)).
:- use_module(library(pair_ext)).





%! s_edge_endpoint(+Edge:pair, +Endpoint) is semidet.
%! s_edge_endpoint(+Edge:pair, -Endpoint) is multi.
% The **endpoints** of an edge are its head and tail.
%
% Notice that for reflexive edges the same endpoint is returned twice.

s_edge_endpoint(E, Head) :- s_edge_head(E, Head).
s_edge_endpoint(E, Tail) :- s_edge_head(E, Tail).



%! s_edge_head(+Edge:pair, +Head) is semidet.
%! s_edge_head(+Edge:pair, -Head) is det.
% The head of a directed edge is the vertex the edge is "pointing to".

s_edge_head(E, Head) :- pair_value(E, Head).



%! s_edge_string(+Edge:pair, -String:string)// is det.

s_edge_string(E, String) :- string_phrase(s_edge_string(E), String).
s_edge_string(V-W) --> term(V), " <---> ", term(W).



%! s_edge_tail(+Edge:pair, +Tail) is semidet.
%! s_edge_tail(+Edge:pair, -Tail) is det.
% The tail of a directed edge is the vertex the edge is "pointing from".

s_edge_tail(E, Tail) :- pair_key(E, Tail).



%! s_edges_vertices(+Edges:list(pair), -Vertices:ordset) is det.
% Returns the vertices that occur in the given edges.

s_edges_vertices(Es, Vs) :-
  pairs_keys_values(Es, Vs1a, Vs2a),
  list_to_ord_set(Vs1a, Vs1b),
  list_to_ord_set(Vs2a, Vs2b),
  ord_union(Vs1b, Vs2b, Vs).
