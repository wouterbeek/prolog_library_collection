:- module(
  graph_traverse,
  [
    traverse/6, % :G2Es_2
                % :V2N_3
                % +G
                % +V
                % ?W
                % :Options:list(compound)
    traverse/7, % :G2V_2
                % :G2Es_2
                % :V2N_3
                % +G
                % ?V
                % ?W
                % :Options:list(compound)
    traverse/9, % :G2Es_2
                % :V2N_3
                % +G
                % +V
                % ?W
                % -Vs:ordset
                % -Es:ordset(pair)
                % -History:list
                % :Options:list(compound)
    traverse/10, % :G2V_2
                 % :G2Es_2
                 % :V2N_3
                 % +G
                 % ?V
                 % ?W
                 % -Vs:ordset
                 % -Es:ordset(pair)
                 % -History:list
                 % :Options:list(compound)
    traverse_min/7, % :G2Es_2
                    % :V2N_3
                    % +G
                    % +V
                    % +W
                    % -Distance:nonneg
                    % :Options:list(compound)
    traverse_min/10 % :G2Es_2
                    % :V2N_3
                    % +G
                    % +V
                    % +W
                    % -Vs:ordset
                    % -Es:ordset
                    % -History:list
                    % -Distance:nonneg
                    % :Options:list(compound)
  ]
).

/** <module> Graph theory: Traversal

Generic graph traversal algorithm.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- meta_predicate
    traverse(2,3,+,+,-,:),
    traverse(2,2,3,+,?,-,:),
    traverse(2,3,+,?,-,-,-,-,:),
    traverse(2,2,3,+,?,-,-,-,-,:),
    traverse0(2,3,+,?,-,?,+,-,-,-,-,-,+),
    traverse_min(2,3,+,+,-,-,:),
    traverse_min(2,3,+,+,-,-,-,-,-,:).

:- rdf_meta
   traverse(:,:,+,r,r,:),
   traverse(:,:,:,+,r,r,:),
   traverse(:,:,+,r,r,-,-,-,:),
   traverse(:,:,:,+,r,r,-,-,-,:),
   traverse0(:,:,+,r,r,?,+,-,-,-,-,-,+),
   traverse_min(:,:,+,r,r,-,:),
   traverse_min(:,:,+,r,r,-,-,-,-,:).

is_meta(debug_vertex_name).

:- predicate_options(traverse/6, 6, [
     pass_to(traverse/10, 10)
   ]).
:- predicate_options(traverse/7, 7, [
     pass_to(traverse/6, 6)
   ]).
:- predicate_options(traverse/9, 9, [
     pass_to(traverse0/13, 13)
   ]).
:- predicate_options(traverse/10, 10, [
     pass_to(traverse/9, 9)
   ]).
:- predicate_options(traverse0/13, 13, [
     debug_vertex_name(+callable),
     distinct_edges(+boolean),
     distinct_vertices(+boolean),
     edge_distance(-nonneg),
     every_edge(+boolean),
     maximum_depth(+nonneg),
     vertex_distance(-nonneg)
   ]).
:- predicate_options(traverse_min/7, 7, [
     pass_to(traverse_min/10)
   ]).
:- predicate_options(traverse_min/10, 10, [
     pass_to(traverse/9, 9)
   ]).





%! traverse(:G2Es_2, :V2N_3, +G, +V, +W, :Options:list(compound)) is semidet.
%! traverse(:G2Es_2, :V2N_3, +G, +V, -W, :Options:list(compound)) is nondet.
% @see Wrapper around traverse/10

traverse(_, _, _, V, _, _):-
  var(V), !,
  instantiation_error(V).
traverse(G2Es_2, V2N_3, G, V, W, Options):-
  traverse(G2Es_2, V2N_3, G, V, W, _, _, _, Options).

%! traverse(:G2V_2, :G2Es_2, :V2N_3, +G, +V, +W, :Options:list(compound)) is semidet.
%! traverse(:G2V_2, :G2Es_2, :V2N_3, +G, +V, -W, :Options:list(compound)) is nondet.
%! traverse(:G2V_2, :G2Es_2, :V2N_3, +G, -V, -W, :Options:list(compound)) is nondet.
% @see Wrapper around traverse/10

traverse(G2V_2, G2Es_2, V2N_3, G, V, W, Options):-
  call(G2V_2, G, V),
  traverse(G2Es_2, V2N_3, G, V, W, Options).

%! traverse(
%!   :G2Es_2,
%!   :V2N_3,
%!   +G,
%!   +V,
%!   -W,
%!   -Vs:ordset,
%!   -Es:ordset,
%!   -History:list,
%!   :Options:list(compound)
%! ) is nondet.

traverse(G2Es_2, V2N_3, G, V, W, Vs, Es, Hist, Options1):-
  meta_options(is_meta, Options1, Options2),
  option(maximum_depth(MaxDepth), Options2, _VAR),
  traverse0(G2Es_2, V2N_3, G, V, W, MaxDepth, 0, [V], Vs, [], Es, Hist, Options2).

%! traverse(
%!   :G2V_2,
%!   :G2Es_2,
%!   :V2N_3,
%!   +G,
%!   ?V,
%!   -W,
%!   -Vs:ordset,
%!   -Es:ordset,
%!   -History:list,
%!   :Options:list(compound)
%! ) is nondet.
% Lets travel through graph land!
%
% # Definitions
%
% A **walk** is an alternating sequence of vertices and edges,
% starting and ending in a vertex.
% ```prolog
% Options = []
% ```
%
% In a **closed** walk sequence, the first and the last vertex are the same.
%
% A **trail** is a walk with distinct edges.
% ```prolog
% Options = [distinct_edges(true)]
% ```
%
% A **tour** is a closed trail.
% ```prolog
% Options = [distinct_edges(true)]
% ```
%
% A **path** is a trail with distinct vertices.
% ```prolog
% Options = [distinct_edges(true),distinct_vertices(true)]
% ```
%
% A **cycle** is a closed path.
% ```prolog
% Options = [distinct_edges(true),distinct_vertices(true)]
% ```
%
% An **Euler tour** is a tour in which all edges are traversed exactly one.
% ```prolog
% Options = [every_edge(true),distinct_edges(true)]
% ```
%
% # Options
%
% The following options are supported:
%   1.  `debug(+boolean)`
%   2.  `debug_vertex_name(+callable)`
%       Debugging option for assigning readable names to vertices.
%       No default.
%   3.  `distinct_edges(+boolean)=
%       Trails and Euler tours have distinct edges.
%       Default: `false`.
%   4.  `distinct_vertices(+boolean)`
%       Cycles and paths have distinct vertices.
%       Default: `false`.
%   5.  `edge_distance(-nonneg)`
%       The number of traversed edges.
%   6.  `euler(+boolean)`
%   7.  `every_edge(+boolean)`
%       Euler tours must use every edge.
%       Default: `false`.
%   8.  `every_vertex(+boolean)`
%   9.  `maximum_depth(+nonneg)`
%       Default: uninstantiated.
%   10. `vertex_distance(-nonneg)`
%       The number of traversed vertices.

traverse(G2V_2, G2Es_2, V2N_3, G, V, W, Vs, Es, Hist, Options):-
  call(G2V_2, G, V),
  traverse(G2Es_2, V2N_3, G, V, W, Vs, Es, Hist, Options).

% Done, perform some option-dependent checks
% and calculate some option-dependent statistics.
traverse0(
  G2Es_2,
  _,
  G,
  V,
  W,
  MaxDepth,
  Depth,
  SolVs,
  SolVs,
  SolEs,
  SolEs,
  [W],
  Options
):-
  (   V == W
  ->  true
  ;   (nonvar(MaxDepth) -> Depth =:= MaxDepth ; true)
  ->  W = V
  ), !,

  % In an Euler tour all edges must be visited.
  (   option(every_edge(true), Options)
  ->  call(G2Es_2, G, AllEs),
      ord_subtract(AllEs, SolEs, UntraversedEs),
      ord_empty(UntraversedEs)
  ;   true
  ),

  % Distance metrics are returned as options.
  (   option(edge_distance(DistE), Options2)
  ->  DistE = Depth
  ;   true
  ),
  (   option(vertex_distance(DistV), Options2)
  ->  DistV is Depth + 1
  ;   true
  ).
% Recursion: traversal by visiting neighboring vertices.
traverse0(
  G2Es_2,
  V2N_3,
  G,
  V,
  W,
  MaxDepth,
  Depth1,
  Vs1,
  SolV,
  Es1,
  SolE,
  [V,V-U|Hist],
  Options
):-
  % Step to a neighbor.
  call(V2N_3, G, V, U),

  % Debugging.
  (   debugging(graph_traversal),
      option(debug_vertex_name(VName), Options)
  ->  call(VName, V, VName),
      call(VName, U, UName),
      debug(graph_traversal, '~w\t--->\t~w', [VName,UName])
  ;   true
  ),

  % Check the walk restriction: no duplicate vertices.
  (   option(distinct_vertices(true), Options)
  ->  \+ memberchk(U, Vs1)
  ;   true
  ),

  % Check the trail and Euler tour restriction: no duplicate edges.
  (   option(distinct_edges(true), Options)
  ->  \+ memberchk(V-U, Es1)
  ;   true
  ),

  % Update vertices and edges.
  ord_add_element(Vs1, U, Vs2),
  ord_add_element(Es1, V-U, Es2),

  succ(Depth1, Depth2),
  traverse0(
    G2Es_2,
    V2N_3,
    G,
    U,
    W,
    MaxDepth,
    Depth2,
    Vs2,
    SolV,
    Es2,
    SolE,
    Hist,
    Options
  ).



%! tarvel_min(
%!   :G2Es_2,
%!   :V2N_3,
%!   +G,
%!   +V,
%!   +W,
%!   -EdgeDistance:nonneg,
%!   +Options:list(compound)
%! ) is nondet.

traverse_min(G2Es_2, V2N_3, G, V, W, EDist, Options):-
  traverse_min(G2Es_2, V2N_3, G, V, W, _, _, _, EDist, Options).

%! traverse_min(
%!   :G2Es_2,
%!   :V2N_3,
%!   +G,
%!   +V,
%!   +W,
%!   -Vs:ordset,
%!   -Es:ordset,
%!   -History:list,
%!   -EdgeDistance:nonneg,
%!   +Options:list(compound)
%! ) is nondet.
% Returns the minimum distance between V and W.

traverse_min(
  G2Es_2,
  V2N_3,
  G,
  V,
  W,
  Vs,
  Es,
  Hist,
  EDist,
  Options1
):-
  meta_options(is_meta, Options1, Options2),
  aggregate_all(
    set(Dist-args(Vs,Es,Hist)),
    (
      merge_options([edge_distance(Dist)], Options2, Options3),
      traverse(G, G2Es_2, V2N_3, V, W, Vs, Es, Hist, Options3)
    ),
    Pairs
  ),
  member([EDist-args(Vs,Es,Hist)|_], Pairs),
  (   option(edge_distance(EDist0), Options3)
  ->  EDist0 = EDist
  ;   true
  ).
