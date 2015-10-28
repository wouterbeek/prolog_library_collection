:- module(
  s_edge,
  [
    s_adjacent_edge/3, % +Graph:ugraph
                       % ?Edge1:pair
                       % ?Edge2:pair
    s_endpoint/2, % +Edge:pair
                  % ?Endpoint
    s_graph_head/2, % +Graph:ugraph
                    % ?Head
    s_graph_tail/2, % +Graph:ugraph
                    % ?Tail
    s_link/2, % +Graph:ugraph
              % ?Link:pair
    s_loop/2, % +Graph:ugraph
              % ?Loop:pair
    s_unsymmetric_edge/2 % +Graph:ugraph
                         % ?Edge:pair
  ]
).

/** <module> Graph theory: Edges

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(graph/s/s_test)).
:- use_module(library(plunit)).





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
  s_test(GName, G),
  aggregate_all(set(E), s_adjacent_edge(G, 1-2, E), Es).

s_adjacent_edge_test("p3", [2-1,2-3,3-2], true).

:- end_tests('s_adjacent_edge/3').



%! s_endpoint(+Edge:pair, +Endpoint) is semidet.
%! s_endpoint(+Edge:pair, -Endpoint) is multi.
% For reflexive edges, the same vertex is returned twice.

s_endpoint(V-_, V).
s_endpoint(_-V, V).



%! s_graph_head(+Graph:ugraph:ugraph, +Head) is semidet.
%! s_graph_head(+Graph:ugraph:ugraph, -Head) is nondet.

s_graph_head(G, Head):-
  s_edge(G, _-Head).



%! s_graph_tail(+Graph:ugraph, +Tail) is semidet.
%! s_graph_tail(+Graph:ugraph, -Tail) is nondet.

s_graph_tail(G, Tail):-
  s_edge(G, Tail-_).



%! s_link(+Graph:ugraph, +Link:pair) is semidet.
%! s_link(+Graph:ugraph, -Link:pair) is nondet.
% A *link* is a non-reflexive edge.

% Optimization for the semidet case where `Link` is a loop.
s_link(G, V-W):-
  s_edge(G, V-W),
  V \== W.



%! s_loop(+Graph:ugraph, +Loop:pair) is semidet.
%! s_loop(+Graph:ugraph, -Loop:pair) is nondet.
% A *loop* is a reflexive edge.

s_loop(G, V-V):-
  s_edge(G, V-V).



%! s_unsymmetric_edge(+Graph:ugraph, +Edge:pair) is semidet.
%! s_unsymmetric_edge(+Graph:ugraph, -Edge:pair) is nondet.
% For every pair of symmetric edges $\set{\tuple{V, W}, \tuple{W, V}}$
% we only take the edge for which the first member is smaller than the
% latter.
%
% *|Special case|*: Reflexive edges are symmetric and therefore removed
%                   entirely.

s_unsymmetric_edge(G, V-W):-
  s_edge(G, V-W),
  \+ s_edge(G, W-V).
