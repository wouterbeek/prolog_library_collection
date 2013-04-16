:- module(
  rdf_graph_theory,
  [
    rdf_edge/2, % +Options:list(nvpair)
                % ?Edge:edge
    rdf_graph_to_ugraph/2, % +Graph:atom
                           % -UG:ugraph
    rdf_subgraph/3, % +Options:list(nvpair)
                    % +SubVertices:ord_set(vertex)
                    % +SubGraph:graph
    rdf_neighbor/3, % +Options:list(nvpair)
                    % ?Vertex:vertex
                    % ?Neighbor:vertex
    rdf_vertex/2, % +Options:list(nvpair)
                  % ?Vertex:vertex
    rdf_vertex_equivalence/2 % +Resource1:uri
                             % +Resource2:uri
  ]
).

/** <module> RDF graph theory

Graph theory support for RDF.

Graph theoretic insights cannot be directly applied to RDF graphs because
edges (as defined by RDF abstract syntax) in one triple can be nodes in
another. This means that the definitions 'edge' and 'vertex' for graph
theoretic operations of RDF data must be redefined.

@author Wouter Beek
@version 2012/01-2013/03
*/

:- use_module(generic(meta_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_read)). % Used for meta-calls.

:- rdf_meta(rdf_edge(+,r)).
:- rdf_meta(rdf_neighbor(+,r,r)).
:- rdf_meta(rdf_vertex(+,r)).
:- rdf_meta(rdf_vertex_equivalence(r,r)).



%% rdf_edge(+Options:list(nvpair), ?Edge:edge) is nondet.
% RDF edges between resources.
%
% @param Options A list of the following name-value pairs:
%        1. =|directed(DirectedGraph:boolean)|= Whether or not the
%           directionality of the edge is taken into account.
%        2. =|graph(Graph:atom)|= The atomic name of the graph to which =Edge=
%           must belong.
%        3. =|literals(Include:boolean)|= Whether or not literals are
%           allowed as vertices in the =Edge=.
%           Default: =false=.
% @param Edge An edge, either =|From-To|= or =|From-Predicate-To|=.

rdf_edge(Options, From-To):-
  % Edges within a certain graph.
  option(graph(Graph), Options, user),
  % Whether the edge's directionality is relevant or not.
  option(directed(Directed), Options, false),
  (
    Directed == true
  ->
    rdf(From, Predicate, To, Graph)
  ;
    (
      rdf(From, Predicate, To, Graph)
    ;
      rdf(To, Predicate, From, Graph)
    )
  ),
  % Make sure the vertices pass the vertex filter.
  rdf_vertex(Options, From),
  rdf_vertex(Options, To),
  % Whether or not to include literal vertices.
  option(literals(Literals), Options, collapse),
  (
    Literals == show
  ->
    true
  ;
    % 'Literals == collapse' or 'Literals == hide'.
    From \= literal(_),
    To \= literal(_)
  ).

%% rdf_graph_to_ugraph(+G:atom, -UG:ugraph) is det.
% Returns the UG representation of a loaded RDF graph.
%
% @param G The atomic name of a loaded RDF graph.
% @param UG:ugraph A UG datastructure.

rdf_graph_to_ugraph(G, UG):-
  setoff(
    From-Neighbors,
    (
      rdf_vertex([graph(G)], From),
      setoff(
        To,
        rdf_edge([graph(G)], From-To),
        Neighbors
      )
    ),
    UG
  ).

%% rdf_neighbor(
%%   +Options:list(nvpair),
%%   ?Vertex:vertex,
%%   ?Neighbor:vertex
%% ) is nondet.
% Neighboring vertex.
%
% @param Options A list of the following name-value pairs:
%        1. =graph(Graph:atom)= The atomic name of the graph to which =Edge=
%           must belong.
%        2. =literals(IncludeLiterals:boolean)= Whether or not literals are
%           allowed as vertices in the =Edge=.

rdf_neighbor(Options0, Vertex, Neighbor):-
  merge_options([literals(show)], Options0, Options),
  rdf_edge(Options, Vertex-Neighbor).

%% rdf_subgraph(
%%   +Options:list(nvpair),
%%   +Vertices:ord_set(vertice),
%%   +SubGraph:graph
%% ) is semidet.
% Succeeds if the graph in Options has SubGraph as one of its vertice-induced
% subgraph.

rdf_subgraph(Options, SubVertices, SubGraph):-
  % Make sure that the given set of vertices is a subset of the graph in
  % Options.
  vertices1(Options, Vertices),
  ord_subset(SubVertices, Vertices),
  option(graph(Graph), Options),
  forall(
    (
      rdf(S, P, O, Graph),
      member(S, SubVertices),
      member(O, SubVertices)
    ),
    rdf_assert(S, P, O, SubGraph)
  ).

%% rdf_vertex(+Options:list(nvpair), ?Vertex:uri) is nondet.
% Pairs of graphs and nodes that occur in that graph.
% A node is either a subject or an object term in an
% RDF triple.
%
% @param Options A list of name-value pairs.
%        1. =graph(Graph:atom)= The atomic name of a graph.
%        2. =|literals(oneof([collapse,hide,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Default: =collapse=.
%        3. =|rdf_list(onef([concise,full]))|= Whether vertices that are part
%           of an RDF list should be included or not.
%           Default: =full=.
%        4. =|vertex(oneof([rdf_node,rdf_term])|=
%           Value =rdf_node= means that only subject and object terms are
%           considered as vertices.
%           Value =rdf_term= means that subject, predicate and object terms
%           are considered as vertices.
%           Default: =rdf_node=.% @param Node A resource.

rdf_vertex(Options, Vertex):-
  option(graph(Graph), Options),

  % Nodes or terms?
  option(vertex(VerticePredicate), Options, rdf_node),
  call(VerticePredicate, Graph, Vertex),

  % Literals.
  option(literals(Literals), Options, collapse),
  (
    Literals == show
  ->
    true
  ;
    % 'Literal == collapse' or 'Literal == hide'.
    Vertex \= literal(_Literal)
  ),

  % RDF lists.
  option(rdf_list(RDF_List), Options, full),
  (
    RDF_List = concise
  ->
    (
      rdf(Vertex, rdf:type, rdf:'List', Graph)
    ->
      % Only RDF list vertices that are list heads are included.
      \+ rdf(_, rdf:rest, Vertex, Graph)
    ;
      % Non-RDF list vertices should not occur as member of an RDF list.
      \+ rdf(_, rdf:first, Vertex, Graph)
    )
  ;
    true
  ).

rdf_vertex_equivalence(X, Y):-
  % Subject
  forall(
    rdf(X, P, O, _GX1),
    rdf(Y, P, O, _GY1)
  ),
  forall(
    rdf(Y, P, O, _GY2),
    rdf(X, P, O, _GX2)
  ),
  % Predicate
  forall(
    rdf(S, X, O, _GX3),
    rdf(S, Y, O, _GY3)
  ),
  forall(
    rdf(S, Y, O, _GY4),
    rdf(S, X, O, _GX4)
  ),
  % Object
  forall(
    rdf(S, P, X, _GX5),
    rdf(S, P, Y, _GY5)
  ),
  forall(
    rdf(S, P, Y, _GY6),
    rdf(S, P, X, _GX6)
  ).

