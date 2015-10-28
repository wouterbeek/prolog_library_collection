:- module(
  graph_walk,
  [
    chain/5, % :G2Es_2
             % :V2N_3
             % +G
             % +V
             % ?W
    chain/6, % :G2V_2
             % :G2Es_2
             % :V2N_3
             % +G
             % ?V
             % ?W
    chain/7, % :G2V_2
             % :G2Es_2
             % :V2N_3
             % +G
             % ?V
             % ?W
             % -Chain:list
    cycle/4, % :G2Es_2
             % :V2N_3
             % +G
             % +V
    cycle/5, % :G2V_2
             % :G2Es_2
             % :V2N_3
             % +G
             % ?V
    cycle/6, % :G2V_2
             % :G2Es_2
             % :V2N_3
             % +G
             % ?V
             % -Cycle:list
    path/5, % :G2Es_2
            % :V2N_3
            % +G
            % +V
            % ?W
    path/6, % :G2V_2
            % :G2Es_2
            % :V2N_3
            % +G
            % ?V
            % ?W
    path/7, % :G2V_2
            % :G2Es_2
            % :V2N_3
            % +G
            % ?V
            % ?W
            % -Path:list
    trail/5, % :G2Es_2
             % :V2N_3
             % +G
             % +V
             % ?W
    trail/6, % :G2V_2
             % :G2Es_2
             % :V2N_3
             % +G
             % ?V
             % ?W
    trail/7, % :G2V_2
             % :G2Es_2
             % :V2N_3
             % +G
             % ?V
             % ?W
             % -Trail:list
    walk/5, % :G2Es_2
            % :V2N_3
            % +G
            % +V
            % ?W
    walk/6, % :G2V_2
            % :G2Es_2
            % :V2N_3
            % +G
            % ?V
            % ?W
    walk/7 % :G2V_2
           % :G2Es_2
           % :V2N_3
           % +G
           % ?V
           % ?W
           % -Walk:list
  ]
).

/** <module> Graph traversal

Graph traversal defined on top of a generic graph traversal algorithm.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(graph/graph_traverse)).

:- meta_predicate(chain(2,3,+,+,?)).
:- meta_predicate(chain(2,2,3,+,?,?)).
:- meta_predicate(chain(2,2,2,+,?,?,-)).
:- meta_predicate(cycle(2,3,+,+)).
:- meta_predicate(cycle(2,2,3,+,?)).
:- meta_predicate(cycle(2,2,3,+,?,-)).
:- meta_predicate(path(2,3,+,+,?)).
:- meta_predicate(path(2,2,3,+,?,?)).
:- meta_predicate(path(2,2,3,+,?,?,-)).
:- meta_predicate(trail(2,3,+,+,?)).
:- meta_predicate(trail(2,2,3,+,?,?)).
:- meta_predicate(trail(2,2,3,+,?,?,-)).
:- meta_predicate(walk(2,3,+,+,?)).
:- meta_predicate(walk(2,2,3,+,?,?)).
:- meta_predicate(walk(2,2,3,+,?,?,-)).





%! chain(:G2Es_2, :V2N_3, +G, +V, +W) is semidet.
%! chain(:G2Es_2, :V2N_3, +G, +V, -W) is nondet.
% @see chain/7

chain(G2Es_2, V2N_3, G, V, W):-
  path(G2Es_2, V2N_3, G, V, W).

%! chain(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W) is semidet.
%! chain(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W) is nondet.
%! chain(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W) is nondet.
% @see chain/7

chain(G2V_2, G2Es_2, V2N_3, G, V, W):-
  path(G2V_2, G2Es_2, V2N_3, G, V, W).

%! chain(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W, -Chain:list) is nondet.
%! chain(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W, -Chain:list) is nondet.
%! chain(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W, -Chain:list) is nondet.
% **Chain** and path are the same concept.

chain(G2V_2, G2Es_2, V2N_3, G, V, W, Chain):-
  path(G2V_2, G2Es_2, V2N_3, G, V, W, Chain).



%! cycle(:G2Es_2, :V2N_3, +G, +V) is semidet.
% @see cycle/6

cycle(G2Es_2, V2N_3, G, V):-
  path(G2Es_2, V2N_3, G, V, V).

%! cycle(:G2V_2, :G2Es_2, :V2N_3, +G, +V) is semidet.
%! cycle(:G2V_2, :G2Es_2, :V2N_3, +G, -V) is nondet.
% @see cycle/6

cycle(G2V_2, G2Es_2, V2N_3, G, V):-
  cycle(G2V_2, G2Es_2, V2N_3, G, V, _).

%! cycle(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -Cycle:list) is nondet.
%! cycle(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -Cycle:list) is nondet.
% Succeeds if Cycle is a directed cycle containing the given vertex.
%
% Cycle is a list of (alternating) vertices and edges that form the cycle.
%
% *Definition*: A directed cycle is a directed closed trail where all vertices
%               are unique (as in a directed path) but with V_0 = V_n.

cycle(G2V_2, G2Es_2, V2N_3, G, V, Cycle):-
  path(G2V_2, G2Es_2, V2N_3, G, V, V, Cycle).



%! path(:G2Es_2, :V2N_3, +G, +V, +W) is semidet.
%! path(:G2Es_2, :V2N_3, +G, +V, -W) is nondet.
% @see path/7

path(G2Es_2, V2N_3, G, V, W):-
  traverse(
    G2Es_2,
    V2N_3,
    G,
    V,
    W,
    _,
    _,
    _,
    [distinct_edges(true),distinct_vertices(true)]
  ).

%! path(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W) is semidet.
%! path(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W) is nondet.
%! path(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W) is nondet.
% @see path/7

path(G2V_2, G2Es_2, V2N_3, G, V, W):-
  path(G2V_2, G2Es_2, V2N_3, G, V, W, _).

%! path(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W, -Path:list) is nondet.
%! path(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W, -Path:list) is nondet.
%! path(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W, -Path:list) is nondet.
% A *path* is a trail with distinct vertices.

path(G2V_2, G2Es_2, V2N_3, G, V, W, Path):-
  traverse(
    G2V_2,
    G2Es_2,
    V2N_3,
    G,
    V,
    W,
    _,
    _,
    Path,
    [distinct_edges(true),distinct_vertices(true)]
  ).



%! trail(:G2Es_2, :V2N_3, +G, +V, +W) is semidet.
%! trail(:G2Es_2, :V2N_3, +G, +V, -W) is nondet.
% @see trail/7

trail(G2Es_2, V2N_3, G, V, W):-
  traverse(G2Es_2, V2N_3, G, V, W, _, _, _, [distinct_edges(true)]).

%! trail(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W) is semidet.
%! trail(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W) is nondet.
%! trail(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W) is nondet.
% @see trail/7

trail(G2V_2, G2Es_2, V2N_3, G, V, W):-
  trail(G2V_2, G2Es_2, V2N_3, G, V, W, _).

%! trail(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W, -Trail:list) is nondet.
%! trail(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W, -Trail:list) is nondet.
%! trail(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W, -Trail:list) is nondet.
% A *trail* is a walk with distinct edges.

trail(G2V_2, G2Es_2, V2N_3, G, V, W, Trail):-
  traverse(
    G2V_2,
    G2Es_2,
    V2N_3,
    G,
    V,
    W,
    _,
    _,
    Trail,
    [distinct_edges(true)]
  ).



%! walk(:G2Es_2, :V2N_3, +G, +V, +W) is nondet.
%! walk(:G2Es_2, :V2N_3, +G, +V, -W) is semidet.
% @see walk/7

walk(G2Es_2, V2N_3, G, V, W):-
  traverse(G2Es_2, V2N_3, G, V, W, _, _, _, []).

%! walk(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W) is nondet.
%! walk(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W) is semidet.
%! walk(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W) is semidet.
% @see walk/7

walk(G2V_2, G2Es_2, V2N_3, G, V, W):-
  walk(G2V_2, G2Es_2, V2N_3, G, V, W, _).

%! walk(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W, -Walk:list) is nondet.
%! walk(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W, -Walk:list) is nondet.
%! walk(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W, -Walk:list) is nondet.
% *Definition*: A **walk** is an alternating sequence of
%               vertices and edges, starting and ending in a vertex.

walk(G2V_2, G2Es_2, V2N_3, G, V, W, Walk):-
  traverse(G2V_2, G2Es_2, V2N_3, G, V, W, _, _, Walk, []).
