:- module(
  graph_web,
  [
    circle_graph_web/2, % +Graph:graph
                        % -Markup:list
    graph_web/2, % +Graph:graph
                 % -Markup:list
    harary_web/3, % +K:integer
                  % +N:integer
                  % -Markup:list
    random_graph_web/2, % +Graph:graph
                        % -Markup:list
    schema_web/2, % +Graph:graph
                  % -Markup:list
    spring_embedding_web/2, % +Graph:graph
                            % -Markup:list
    spring_embedding_web/3, % +Graph:graph
                            % +Iterations:integer
                            % -Markup:list
    table_graph_web/2, % +Graph:atom
                       % -Markup:element
    vertex_web/3 % +Graph:atom
                 % +Vertex:atom
                 % -Markup:list
  ]
).

/** <module> Graph Web

Web front-end for generic graph visualizations.

@author Wouter Beek
@version 2013/01-2013/02
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(exception_handling)).
:- use_module(generics(os_ext)).
:- use_module(graph_theory(graph_export)).
:- use_module(graph_theory(graph_generic)).
:- use_module(graph_theory(spring_embedding)).
:- use_module(graph_theory(vertex_coordinate)).
:- use_module(graph_theory(ugraphs_ext)).
:- use_module(server(error_web)).
:- use_module(standards(graphviz)).



%% circle_graph_web(+Graph:graph, -Markup:list) is det.

circle_graph_web(Graph, html/prasem/[GraphElement | TableMarkup]):-
  export_graph(
    [
      edge_labels(all),
      out(svg),
      vertex_coordinate(circular_vertice_coordinate)
    ],
    Graph,
    GraphElement
  ),
  table_graph_web(Graph, _DTD_Name/_StyleName/TableMarkup).

%% graph_web(+Graph:graph, -Markup:list) is det.
% Writes the graph with the given name to a file in GraphViz DOT format.
%
% @param Graph
% @param Markup

graph_web(Graph, svg11/prasem/SVG):-
  write_graph(
    [
      colorscheme(svg),
      edge_labels(replace),
      in(rdf),
      language(en),
      literals(collapse),
      out(graphviz),
      rdf_list(concise),
      vertex(rdf_node)
    ],
    Graph,
    GraphViz_File
  ),
  graphviz_to_svg(GraphViz_File, sfdp, SVG).

%% harary_web(+K:integer, +N:integer, -Markup:list) is det.
% Returns markup represening the K-connected Harary graph with N vertices.

harary_web(K, N, Markup):-
  harary(K, N, Harary),
  circle_graph_web(Harary, Markup).

%% random_graph_web(+Graph:graph, -Markup:list) is det.
% Returns an Web representation of the RDF graph with the given title.
% Graphs that are loaded via this front-end should be located in the user's
% =data= subdirectory.
%
% @param Graph
% @param Markup

random_graph_web(Graph, [GraphElement, TableElement]):-
  random_vertice_coordinates([graph(Graph)], RandomVerticeCoordinates),
  vertice_coordinates_web(Graph, RandomVerticeCoordinates, GraphElement),
  vertice_coordinates_table(RandomVerticeCoordinates, TableElement).

schema_web(Graph, SVG):-
  write_schema(
    [
      directed(true),
      edge_labels(all),
      graph(Graph),
      in(rdf),
      literals(collapse),
      out(graphviz)
    ],
    Graph,
    graph,
    GraphViz_File
  ),
  graphviz_to_svg(GraphViz_File, sfdp, SVG).

%% spring_embedding_web(+Graph:graph, -Markup:list) is det.
% @see spring_embedding_web/3

spring_embedding_web(Graph, Markup):-
  spring_embedding_web(Graph, 50, Markup).

%% spring_embedding_web(
%%   +Graph:graph,
%%   +Iterations:integer,
%%   -Markup:list
%% ) is det.
% Returns the markup for the spring embedding of the given graph.
%
% @param Graph
% @param Iterations An integer, representing the number of iterations
%        of spring embedding, i.e., the number of subsequent function
%        applications.
% @param Markup A list of markup elements.

spring_embedding_web(Graph, Iterations, Markup):-
  default_spring_embedding(
    Graph,
    Iterations,
    _FinalVerticeCoordinates,
    History
  ),
  findall(
    MarkupElement,
    (
      member(VerticeCoordinates, History),
      vertice_coordinates_web(Graph, VerticeCoordinates, MarkupElement)
    ),
    Markup
  ).

%% table_graph_web(+Graph:graph, -Markup:triple) is det.

table_graph_web(Graph, html/prasem/[TableElement]):-
  export_graph(
    [
      colorscheme(none),
      edge_labels(all),
      in(rdf),
      literals(collapse),
      out(html_table)
    ],
    Graph,
    TableElement
  ).

%% vertex_web(+Graph:atom, +Vertex:vertex, -Markup:dom) is det.
% Creates a DOM representation for the given vertex in the given graph.
%
% @param Graph The atomic name of an RDF graph.
% @param Vertex
% @param Markup

vertex_web(Graph, Vertex0, SVG):-
  term_atom(Vertex0, Vertice1),
  rdf_global_id(Graph:Vertice1, Vertex),
  write_vertex(
    [
      directed(true),
      edge_labels(all),
      graph(Graph),
      in(rdf),
      literals(collapse),
      out(graphviz)
    ],
    Vertex,
    vertex,
    GraphViz_File
  ),
  graphviz_to_svg(GraphViz_File, sfdp, SVG).

