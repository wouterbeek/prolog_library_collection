:- module(
  graph_generic,
  [
    beam/5, % +Options:list(nvpair)
            % +Vertex:vertex
            % +Predicates:list(predicate)
            % -Vertices:ord_set(vertex)
            % -Edges:ord_set(edge)
    betweenness/3, % +G:graph
                   % +V:vertex
                   % -Betweenness:float
    connected/1, % +Options:list(nvpair)
    cubic/1, % +Graph:ugraph
    degree/3, % +Options:list(nvpair)
              % +Vertex:vertex
              % -Degree:integer
    degree_sequence/2, % +Options:list(nvpair)
                       % -DegreeSequence:list(integer)
    depth/5, % +Options:list(nvpair)
             % +Vertex:vertex
             % +Depth:integer
             % -Vertices:ord_set(vertex)
             % -Edges:ord_set(edge)
    edge/2, % +Options:list(nvpair)
            % ?Edge:edge
    edge_coloring/3, % +Options:list(nvpair)
                     % +Edge:edge
                     % -Color:color
    edge_naming/3, % +Options:list(nvpair)
                   % +Edge:edge
                   % -Name:atom
    edge_styling/3, % +Options:list(nvpair)
                    % +Edge:edge
                    % -Style:pair(oneof([bold,dashed,dotted,solid]),atom)
    edges1/2, % +Options:list(nvpair)
              % -Edges:ord_set(edge)
    graph/3, % ?G:graph
             % ?V:ord_set(vertice)
             % ?E:ord_set(edge)
    graph_format/2, % +Graph:graph
                    % -Format:oneof([rdf,ugraph])
    graph_naming/2, % +Options:list(nvpair)
                    % -Name:atom
    graph_to_ugraph/2, % +Graph:graph
                       % -UGraph:ugraph
    graphic/1, % +S:list(integer)
    has_cycle/1, % +Options:list(nvpair)
    neighbor/3, % +Options:list(nvpair)
                % ?Vertex:vertex
                % ?Neighbor:vertex
    neighbors/3, % +Options:list(nvpair)
                 % +Vertex:vertex
                 % -Neighbors:ord_set(vertex)
    regular/1, % +Graph:ugraph
    regular/2, % +Graph:ugraph
               % ?K:integer
    shortest_paths/5, % +G:graph
                      % +X:vertex
                      % +Y:vertex
                      % +V:vertex
                      % -ShortestPaths:list(path)
    simple/1, % +Options:list(nvpair)
    strict_subgraph/2, % +Options:list(nvpair)
                       % ?StrictSubGraph:graph
    subgraph/2, % +Options:list(nvpair)
                % +SubGraph:graph
    travel/4, % +Options:list(nvpair)
              % +First:vertex
              % +Last:vertex
              % -Distance:integer
    travel/7, % +Options:list(nvpair)
              % +First:vertex
              % +Last:vertex
              % -Distance:integer
              % -Vertices:ord_set(vertex)
              % -Edges:ord_set(edge)
              % -History:list
    travel_min/4, % +Options:list(nvpair)
                  % +First:vertex
                  % +Last:vertex
                  % -MinimumDistance:integer
    travel_min/7, % +Options:list(nvpair)
                  % +First:vertex
                  % +Last:vertex
                  % -MinimumDistance:integer
                  % -Vertices:ord_set(vertex)
                  % -Edges:ord_set(edge)
                  % -History:list
    vertex/2, % +Options:list(nvpair)
              % ?Vertex:vertex
    vertex_coloring/3, % +Options:list(nvpair)
                       % +Vertex:vertex
                       % -Color:atom
    vertex_naming/3, % +Options:list(nvpair)
                     % +Vertex:vertex
                     % -Name:atom
    vertex_picturing/3, % +Options:list(nvpair)
                        % +Vertex:vertex
                        % -Image:atom
    vertex_shaping/3, % +Options:list(nvpair)
                      % +Vertex:vertex
                      % -Shape:list
    vertices1/2 % +Options:list(nvpair)
                % -Vertices:ord_set(vertex)
  ]
).

/** <module> Generic graph methods

---+ Which graph format?

graph_format/2
    1. =rdf=
    2. =ugraph=

---+ Datatypes

---++ Edge

---++ Graph

---++ Vertex

@author Wouter Beek
@version 2013/01-2013/04
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(type_checking)).
:- use_module(graph_theory(ugraphs_ext)).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_export)). % Meta-predicates.
:- use_module(rdf(rdf_graph_theory)).

:- rdf_meta(beam(+,r,+,-,-)).
:- rdf_meta(degree(+,r,-)).
:- rdf_meta(travel(+,r,r,-)).
:- rdf_meta(travel(+,r,r,-,-,-,-)).
:- rdf_meta(travel_min(+,r,r,-)).
:- rdf_meta(travel_min(+,r,r,-,-,-,-)).



beam(Options, Vertex, Predicates, Vertices, Edges):-
  beam(Options, [Vertex], Predicates, Vertices, [], Edges).

beam(_Options, [], _Predicates, AllVertices, AllEdges, AllEdges):-
  edges_to_vertices(AllEdges, AllVertices),
  !.
beam(Options, Vertices, Predicates, AllVertices, Edges, AllEdges):-
  setoff(
    Vertex-NextVertex,
    (
      member(Vertex, Vertices),
      member(Predicate, Predicates),
      rdf_has(Vertex, Predicate, NextVertex),
      \+ member(Vertex-NextVertex, Edges)
    ),
    NextEdges
  ),
  ord_union(Edges, NextEdges, NewEdges),
  edges_to_vertices(NextEdges, NextVertices),
  beam(Options, NextVertices, Predicates, AllVertices, NewEdges, AllEdges).

betweenness(Graph, SortedPairs):-
  vertices1([graph(Graph)], Vs1),
  rdf_global_id(rdfs:'Class', RDFS_Class),
  once(select(RDFS_Class, Vs1, Vs2)),
  map_list_to_pairs(betweenness(Graph), Vs2, Pairs1),
  edges1([graph(Graph)], Edges),
  findall(
    Sum-V/W,
    (
      member(V-W, Edges),
      member(X-V, Pairs1),
      member(Y-W, Pairs1),
      Sum is X + Y
    ),
    Pairs2
  ),
  sort([duplicates(true), inverted(true)], Pairs2, SortedPairs).

%% betweenness(+G:graph, +V:vertex, -Betweenness:float) is det.
% Betweenness centrality.

betweenness(G, V, Betweenness):-
  graph_to_ugraph(G, UG),
  vertices1([graph(UG)], Vs),
  findall(
    O,
    (
      member(X, Y, Vs),
      shortest_paths(UG, X, Y, _, GenericShortestPaths),
      length(GenericShortestPaths, NumberOfGenericShortestPaths),
      shortest_paths(UG, X, Y, V, SpecificShortestPaths),
      length(SpecificShortestPaths, NumberOfSpecificShortestPaths),
      O is NumberOfSpecificShortestPaths / NumberOfGenericShortestPaths
    ),
    Os
  ),
  sum_list(Os, Betweenness).

%% connected(+Options:list(nvpair)) is semidet.
% Succeeds if the given graph is connected.
%
% *Definition*: A graph is connected if all pairs of vertices are connected.
%
% *Definition*: Two vertices are connected if there is a path between them.
% So vertice connectedness is just path existence.
%
% @see connected/3

connected(O1):-
  vertices1(O1, Vertices),
  merge_options([unique_vertex(true)], O1, O2),
  forall(
    member(V, W, Vertices),
    % A path connects vertices V and W.
    travel(O2, V, W, _Distance)
  ).

%% cubic(+Options:list(nvpair)) is semidet.
% Succeeds if the given graph is cubic.
%
% *Definition*: A cubic graph is a 3-regular graph.

cubic(Options):-
  regular(Options, 3).

%% degree(+Options:list(nvpair), +Vertex:vertex, -Degree:integer) is det.
% Returns the degree of the given vertex.
%
% @param Options A list of name-value pairs.
%        1. =graph(Graph:ugraph)=
%           Supported: RDF, UGRAPH.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Supported for: RDF.

degree(Options, Vertex, Degree):-
  neighbors(Options, Vertex, Neighbors),
  length(Neighbors, Degree).

%% degree_sequence(
%%   +Options:list(nvpair),
%%   -DegreeSequence:list(integer)
%% ) is det.
% Returns the degree sequence of the given graph.
%
% @param Options A list of name-value pairs.
%        1. =graph(Graph:ugraph)=
%           Supported: RDF, UGRAPH.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Supported for: RDF.
% @param DegreeSequence A list of integers.

degree_sequence(Options, DegreeSequence):-
  vertices1(Options, Vertices),
  findall(
    Degree,
    (
      member(Vertex, Vertices),
      degree(Options, Vertex, Degree)
    ),
    UnsortedDegreeSequence
  ),
  % Sorting from largest to smallest degree, including duplicates.
  sort(
    [duplicates(true), inverted(true)],
    UnsortedDegreeSequence,
    DegreeSequence
  ).

%% depth(
%%   +Options:list(nvpair),
%%   +Vertex:vertex,
%%   +Depth:integer,
%%   -Vertices:ord_set(vertex),
%%   -Edges:ord_set(edge)
%% ) is det.
% Returns all vertices and edges that are found within the given depth
% distance from the given vertex.
%
% @param Options A list of name-value pairs, consisting of the following:
%        1. =|directed(boolean)|= Whether only outgoing or also incoming arcs
%           are included in the export.
%        2. =graph(Graph:graph)=
%        3. =in(Format:oneof([rdf,ugraph]))=

depth(Options, Vertex, Depth, Vertices, Edges):-
  Depth > 0,
  depth0(Options, [Vertex], Depth, [], Vertices, [], Edges).

depth0(_Options, Vertices, 0, VerticesH, AllVertices, AllEdges, AllEdges):-
  !,
  ord_union(VerticesH, Vertices, AllVertices).
depth0(
  Options,
  CurrentVertices,
  Depth,
  VerticesH,
  AllVertices,
  EdgesH,
  AllEdges
):-
  setoff(
    Vertex-Neighbor,
    (
      member(Vertex, CurrentVertices),
      neighbor(Options, Vertex, Neighbor)
    ),
    CurrentEdges0
  ),
  ord_subtract(CurrentEdges0, EdgesH, CurrentEdges),
  setoff(
    Neighbor,
    member(_Vertex-Neighbor, CurrentEdges),
    Neighbors
  ),
  ord_union(CurrentVertices, VerticesH, NewVerticesH),
  ord_subtract(Neighbors, NewVerticesH, NextVertices),
  NewDepth is Depth - 1,
  ord_union(EdgesH, CurrentEdges0, NewEdgesH),
  depth0(
    Options,
    NextVertices,
    NewDepth,
    NewVerticesH,
    AllVertices,
    NewEdgesH,
    AllEdges
  ).

%% edge(+Options:list(nvpair), ?Edge:edge) is nondet.
% Edges in a UGRAPH.
%
% @param Options A list of the following name-value pairs:
%        1. =directed(DirectedGraph:boolean)= Whether or not the
%           directionality of the edge is taken into account.
%           Supported for: RDF, UGRAPH.
%        2. =graph(Graph:atom)= The atomic name of the graph to which =Edge=
%           must belong.
%           Supported for: RDF, UGRAPH.
%        3. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Default: =collapse=.
%           Supported for: RDF.
%        4. =in(Format:oneof([rdf,ugraph_ext]))=
% @param Edge An edge, of the form =|From-To|=.

edge(Options, Edge):-
  generic(Options, edge, [Edge]).

edge_coloring(Options, Edge, Color):-
  generic(Options, edge_coloring, [Edge, Color]).

%% edge_naming(+Options:list(nvpair), +Edge:edge, -Name:atom) is det.
% Returns a name for the given edge.
%
% @param Options A list of name-value pairs.
%        1. =in(Format:oneof([rdf,ugraph_ext]))=
% @param Edge An edge of the form =|FromVertex-ToVertex|=.
% @param Name An atomic name for the given edge.

edge_naming(Options, Edge, Name):-
  generic(Options, edge_naming, [Edge, Name]).

edge_styling(Options, Edge, Style):-
  generic(Options, edge_styling, [Edge, Style]).

edges_to_vertices(Edges, Vertices):-
  setoff(
    Vertex,
    (
      member(Vertex1-Vertex2, Edges),
      (
        Vertex = Vertex1
      ;
        Vertex = Vertex2
      )
    ),
    Vertices
  ).

%% edges1(+Options:list(nvpair), -Edges:ord_set(edge)) is det.
% Returns the edges in the given graph.
%
% @param Options A list of name-value pairs.
%        1. =|graph(Graph)|=
%           Supported by: RDF, UGRAPH.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether literals are seen as vertices as well.
%           Default: =collapse=.
%           Supported by: RDF.
% @param Edges An ordered set of edges.

edges1(Options, Edges):-
  setoff(Edge, edge(Options, Edge), Edges).

%% graph_generic(
%%   +Options:list(nvpair),
%%   +GenericPredicate:atom,
%%   +OtherArguments:list
%% ) is det.
% Calls a specific predicate based on its generic names and the
% =input_format= option.

graph_generic(Options, GenericPredicate, OtherArguments):-
  (
    option(in(InputFormat), Options)
  ;
    option(graph(Graph), Options),
    graph_format(Graph, InputFormat)
  ),
  !,
  generic(InputFormat, GenericPredicate, [Options | OtherArguments]).

graph(Options, Vertices, Edges):-
  edges1(Options, Edges),
  vertices1(Options, Vertices).

%% graph_format(+Graph:graph, -Format:oneof([rdf,ugraph])) is semidet.
% Succeeds if the representation format of the given graph can be determined.
%
% @param Graph
% @param Format One of the following values:
%        1. =rdf=
%        2. =ugraph=

graph_format(Graph, rdf):-
  catch(rdf_graph(Graph), _Exception, fail),
  !.
graph_format(UGraph, ugraph):-
  is_list(UGraph),
  maplist(ugraphs_ext:is_ugraph_edge, UGraph),
  !.
graph_format(Graph, unrecognized):-
  existence_error(graph, Graph).

graph_format(Options, Format):-
  option(in(Format), Options),
  !.
graph_format(Options, Format):-
  option(graph(Graph), Options),
  graph_format(Graph, Format).

%% graph_naming(+Options:list(nvpair), -GraphName:atom) is det.
% Return a name of a graph in =Options=.
% The following options are required:
%     1. =|graph(Graph:graph)|=
%     2. =|in(Format:oneof([rdf,ugraph]))|=

graph_naming(Options, GraphName):-
  generic(Options, graph_naming, [GraphName]).

graph_to_ugraph(UG, UG):-
  graph_format(UG, ugraph),
  !.
graph_to_ugraph(G, UG):-
  graph_format(G, rdf),
  rdf_graph_to_ugraph(G, UG).

%% graphic(+Seq:list(integer)) is semidet.
% Succeeds if the given degree sequence represents a simple graph.
%
% *Definition*: A sequence is graphic if the integers correspond
%               to the degrees of a simple graph.
%
% Havel-Hakimi theorem: Consider a list $s = [d_1, \ldots, d_n]$ of $n$
% numbers in descending order. This list is graphic iff
% $s^* = [d_1^*, \ldots, d_n^*]$ of $n - 1$ numbers is graph as well, where
% $d_i^* =
%          d_{i + 1} - 1, \text{for} i = 1, \ldots, d_1
%          d_{i + 1}, \text{otherwise}$

graphic(Zeros):-
  repeating_list(0, _Length, Zeros),
  !.
graphic([H | T]):-
  length(T, LT),
  H =< LT,
  length_cut(T, H, T1, T2),
  maplist(succ, NewT1, T1),
  append(NewT1, T2, NewT_),
  sort([duplicates(true), inverted(true)], NewT_, NewT),
  graphic(NewT).

%% has_cycle(+Options:list(nvpair)) is semidet.
% Succeeds if Options contains a graph setting that has a cycle.
%
% @param Options A list of name-value pairs. The following options are
%        supported:
%        1. =|grah(Graph:graph)|=

has_cycle(O1):-
  vertices1(O1, Vertices),
  member(FirstLast, Vertices),
  merge_options(
    [closed(true), unique_edge(true), unique_vertex(true)],
    O1,
    O2
  ),
  travel(O2, FirstLast, FirstLast, _Distance),
  !.

%% neighbor(
%%   +Options:list(nvpair),
%%   ?Vertex:vertex,
%%   ?Neighbor:vertex
%% ) is nondet.
% Neighboring vertices.
%
% @param Options A list of the following name-value pairs:
%        1. =graph(Graph:atom)=
%           Supported for: RDF, UGRAPH.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Supported for: RDF.

neighbor(Options, Vertex, Neighbor):-
  generic(Options, neighbor, [Vertex, Neighbor]).

%% neighbors(
%%   +Options:list(nvpair),
%%   +Vertex:vertex,
%%   -Neighbors:ord_set(vertex)
%% ) is nondet.
% All neighboring vertices.
%
% @param Options A list of the following name-value pairs:
%        1. =graph(Graph:atom)=
%           Supported for: RDF, UGRAPH.
%        2. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Supported for: RDF.

neighbors(Options, FromVertex, ToVertices):-
  setoff(ToVertex, neighbor(Options, FromVertex, ToVertex), ToVertices).

%% regular(+Options:list(nvpair)) is semidet.
% Succeeds if the graph is regular.
%
% *Definition*: In a regular graph each vertex has the same degree.

regular(Options):-
  regular(Options, _K).

%% regular(+Options:list(nvpair), ?K:integer) is semidet.
% Returns the degree of the given graph, if it is regular.
%
% Definition: A graph is regular if all its vertices have the same degree.

regular(Options, K):-
  vertices1(Options, [Vertex | Vertices]),
  degree(Options, Vertex, K),
  forall(
    member(OtherVertex, Vertices),
    degree(Options, OtherVertex, K)
  ).

%% shortest_paths(
%%   +G:graph,
%%   +X:vertex,
%%   +Y:vertex,
%%   +V:vertex,
%%   -ShortestPaths:list(path)
%% ) is det.
% Returns the shortest paths in graph Graph from vertex X to vertex Y,
% passing vertex V.

shortest_paths(G, X, Y, V, ShortestPaths):-
  setoff(
    Length-Path,
    (
      travel([graph(G), unique_vertex(true)], X, Y, Length, Vs, _Es, Path),
      member(V, Vs)
    ),
    KeyValuePairs
  ),
  group_pairs_by_key(KeyValuePairs, Joined),
  (
    Joined == []
  ->
    ShortestPaths = []
  ;
    Joined = [_ShortestDistance-ShortestPaths | _]
  ).

%% simple(+Graph:ugraph) is semidet.
% Generates simple graphs.
%
% *Definition*: A simple graph has no doubly occurring edges and no loops.

simple(O1):-
  \+(has_cycle(O1)).

strict_subgraph(Options, SubGraph):-
  subgraph(Options, SubGraph),
  option(graph(Graph), Options),
  SubGraph \== Graph.

%% subgraph(+Options:list(nvpair), +SubGraph:graph) is semidet.
% Succeeds if the graph in Options has SubGraph as one of its subgraphs.

% Semidet / subgraph checking.
subgraph(Options, SubGraph):-
  nonvar(SubGraph),
  !,
  merge_options([graph(SubGraph)], Options, SubOptions),
  vertices1(Options, Vertices),
  vertices1(SubOptions, SubVertices),
  ord_subset(SubVertices, Vertices),
  edges1(Options, Edges),
  edges1(SubOptions, SubEdges),
  ord_subset(SubEdges, Edges).
% Nondet / subclass generator.
subgraph(Options, SubGraph):-
  option(graph(G), Options),
  graph_to_ugraph(G, UG),
  ugraph_subgraph(SubGraph, UG).

%% travel(
%%   +Options:list(nvpair),
%%   +First:vertex,
%%   +Last:vertex,
%%   -Distance:uinteger
%% ) is nondet.
% Works together with maplist/4 and maplist_pairs/3 calls.
%
% @see travel/7

travel(Options, First, Last, Distance):-
  travel(Options, First, Last, Distance, _Vertexs, _Edges, _History).

%% travel(
%%   +Options:list(nvpair),
%%   +First:vertex,
%%   +Last:vertex,
%%   -Distance:integer
%%   -Vertices:ord_set(vertex),
%%   -Edges:ord_set(edge),
%%   -History:list
%% ) is nondet.
% Lets travel through graph land.
%
% A *walk* is an alternating sequence of vertices and edges, ending in a
% vertex.
%
% A *tour* is a closed walk. Closed means that the first and the last element
% in the sequence are the same vertex.
% Options = [closed(true)]
%
% A *trail* is a walk with unique edges.
% Options = [unique_edge(true)]
%
% A *path* is a walk with unique vertices.
% Options = [unique_vertex(true)]
%
% A *cycle* is a closed path trail.
% Options = [closed(true), unique_edge(true), unique_vertex(true)]
%
% An *Euler tour* is a tour in which all edges are traversed exactly one.
% Options = [closed(true), every_edge(true), unique_edge(true)]
%
% @param Options A list of name-value pairs. The following options are
%        defined:
%        1. =|closed(boolean)|=
%        2. =|distance(oneof([edge,vertex]))|= For statiscs we return either
%           the number of edges or the number of vertices that were traversed.
%           Default: =edge=.
%        3. =|euler(boolean)|=
%        4. =|every_edge(boolean)|=
%        5. =|every_vertex(boolean)|=
%        6. =|graph(Graph)|=
%        7. =|unique_edge(boolean)=
%        8. =|unique_vertex(boolean)=
% @param First The first vertex in the path.
% @param Last The last vertex in the path.
% @param Distance An integer representing a distance between the first and
%        the last vertex, counted as the number of traversed edges.
% @param Vertices A list of vertices.
% @param Edges A list of edges.
% @param History

travel(Options, First, Last, Distance, Vertices, Edges, History):-
  travel0(
    Options, First, Last,
    Distance, [First], Vertices, [], Edges, History
  ).

travel0(Options, Last, Last, Distance, SolV, SolV, SolE, SolE, [Last]):-
  % Check whether this is a tour, i.e., whether the walk is closed.
  (
    option(close(true), Options, false)
  ->
    % The first and the last vertex must be the same.
    last(SolV, Last)
  ;
    true
  ),

  % Check whether this is an Euler, i.e., all edges were visited.
  (
    option(every_edge(true), Options, false)
  ->
    edges1(Options, AllEdges),
    ord_subtract(AllEdges, SolE, UntraversedEdges),
    ord_empty(UntraversedEdges)
  ;
    true
  ),

  % Distance metric. The statistics we return.
  option(distance(DistanceMetric), Options, edge),
  (
    DistanceMetric == edge
  ->
    length(SolE, Distance)
  ;
    DistanceMetric == vertex
  ->
    length(SolV, Distance)
  ),
  !.
travel0(
  Options, First, Last,
  Distance, Vertices, SolV, Edges, SolE, [First, First-X | History]
):-
  % Neighbor
  neighbor(Options, First, X),

  % Check the walk restriction: no duplicate vertices.
  (
    option(unique_vertex(true), Options, false)
  ->
    \+ member(X, Vertices)
  ;
    true
  ),

  % Check the Euler restriction: no duplicate edges.
  (
    option(trail(true), Options, false)
  ->
    \+ member(First-X, Edges)
  ;
    true
  ),

  ord_add_element(Vertices, X, NewVertices),
  ord_add_element(Edges, First-X, NewEdges),
  travel0(
    Options, X, Last,
    Distance, NewVertices, SolV, NewEdges, SolE, History
  ).

%% tarvel_min(
%%   +Options:list(nvpair),
%%   +First:vertex,
%%   +Last:vertex,
%%   -MinimumDistance:integer
%% ) is det.
% Works together with maplist/4 and maplist_pairs/3 calls.
%
% @see travel_min/7

travel_min(Options, First, Last, MinimumDistance):-
  travel_min(
    Options,
    First,
    Last,
    MinimumDistance,
    _Vertexs,
    _Edges,
    _History
  ).

%% travel_min(
%%   +Options:list(nvpair),
%%   +First:vertex,
%%   +Last:vertex,
%%   -MinimumDistance:integer,
%%   -Vertices:ord_set(vertex),
%%   -Edges:ord_set(edge),
%%   -History:list
%% ) is det.
% Returns the minimum distance between the given subject and predicate terms.
%
% @param Options A list of name-value pairs. See travel/7 for the list
%        of supported options.
% @param First A vertex, the first in the travel.
% @param Last A respource, the last in the travel.
% @param MinimumDistance An integer representing the minimum distance
%        between the first and last vertex. The kind of distances is set
%        in =Options=.
% @param Vertices An ordered set of vertices.
% @param Edges An ordered set of Edges.
% @param History A list representing a minimum travel between the first and
%        last resources.

travel_min(Options, First, Last, MinimumDistance, Vertices, Edges, History):-
  setoff(
    Distance-History,
    travel(Options, First, Last, Distance, Vertices, Edges, History),
    Pairs
  ),
  first(Pairs, MinimumDistance-History).

vertex(Options, Vertex):-
  generic(Options, vertex, [Vertex]).

%% vertex_coloring(+Options:list(nvpair), +Vertex:vertex, -Color:atom) is det.
% Returns a color name for the given vertex.
%
% @param Options A list of name-value pairs.
%        1. =colorscheme(ColorScheme:oneof([none,svg,x11]))= The atomic name
%           of the color scheme from which the color names are drawn.
%           Supported for: RDF.
%        2. =graph(Graph:atom)= The atomic name of a graph.
%           Supported for: RDF.
% @param Vertex A vertex.
% @param Color The atomic name of a color for the given vertex.

vertex_coloring(Options, Vertex, Color):-
  is_uri(Vertex),
  !,
  rdf_vertex_coloring(Options, Vertex, Color).
vertex_coloring(_Options, _Vertex, black).

%% vertex_naming(+Options:list(nvpair), +Vertex, -VertexName:atom) is det.
% Returns a display name for the given RDF graph vertex.
%
% @param Options A list of name-value pairs.
%        1. =graph(Graph:atom)=
%           Supported for: RDF.
%        2. =language(Language:atom)= The atomic tag of the language that is
%           preferred for vertex naming.
%           Defaults to =en=.
%           Supported for: RDF.
%        3. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Default: =collapse=.
%           Supported for: RDF.
% @param Vertex A vertex.
% @param VertexName An atomic name for the vertex.

vertex_naming(Options, Vertex, Name):-
  (rdf_is_resource(Vertex) ; rdf_is_literal(Vertex); rdf_is_bnode(Vertex)),
  !,
  rdf_vertex_naming(Options, Vertex, Name).
vertex_naming(_Options, Vertex, Name):-
  term_to_atom(Vertex, Name).

vertex_picturing(Options, Vertex, Image):-
  generic(Options, vertex_picturing, [Vertex, Image]).

vertex_shaping(Options, Vertex, Shape):-
  is_uri(Vertex),
  !,
  rdf_vertex_shaping(Options, Vertex, Shape).
vertex_shaping(_Options, _Vertex, [peripheries(1), shape(ellipse)]).

%% vertices1(+Options:list(nvpair), -Vertices:ord_set(vertex)) is det.
% Returns the nodes in the graph with the given name.
%
% @param Options A list of name-value pairs.
%        1. =in(Format:oneof([rdf,ugraph]))=
%           Supported for: RDF, UGRAPH.
% @param Vertices An ordered set of vertices.

vertices1(Options, Vertices):-
  setoff(Vertex, vertex(Options, Vertex), Vertices).

