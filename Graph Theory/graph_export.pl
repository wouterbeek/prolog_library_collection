:- module(
  graph_export,
  [
% SETTINGS
    default_border/1, % ?Border:size
    default_surface/1, % ?Surface:size

% EXPORTING
    export_graph/3, % +Options:list(nvpair)
                    % +Graph:graph
                    % -Export:element
    export_triples/3, % +Options:list(nvpair)
                      % +Triples:list(triple)
                      % -Export:element
    export_vertex/3, % +Options:list(nvpair)
                     % +Vertex:vertex
                     % -Export:element

% EXPORT TO DOM
    graph_to_dom/3, % +Options:list(nvpair)
                    % +Graph:graph
                    % -Stream:stream
    triples_to_dom/3, % +Options:list(nvpair)
                      % +Triples:list(triple)
                      % -Stream:stream
    vertex_to_dom/3 % +Options:list(nvpair)
                    % +Vertex:vertex
                    % -Stream:stream
  ]
).

/** <module> Graph export

# Everybody has their reasons

PraSem currently contains the following methods for graph visualization:
    * RDF2DOT in module =rdf_dot= using modules =graphviz=, =rdf_export=,
      =svg=, =x11=.
    * RDF2SVG in module =rdf_web= using RDF2UGRAPh and then UGRAPH2SVG.
    * UGRAPH2SVG in module =ugraphs_ext_web= using module =svg=.
    * RDF2HTML_TABLE in module =rdf_web= using module =html=.

Unifying these methods would be good for the following reasons:
    * Less code, thus better manageability.
    * New features will have to be added only once for multiple input/output
      formats. E.g., RDF2DOT colors the nodes based on namespace, but RDF2SVG
      does not have this feature.

# So lets do it!

We currently have two input formats: =RDF= and =UGRAPH=,
anticipating additional formats in the future.

We currently have three output formats: =DOT=, =HTML_TABLE= and =SVG=,
anticipating additional formats in the future.

We have to come up with a black box in between the input and output layers.

## Input requirements for the black box

From the input formats we require the following things:
    1. The list of vertices.
    2. The list of edges. We assume there a vertex that occurs in an edge
       also occurs in the list of vertices.
    3. A predicate for naming a vertex. This is typically very different
       for UGRAPHs and RDF graphs.
    4. A predicate for colorizing vertices. Whether an output format uses
       these colors is up to the output format.
    5. An argument that specifies the output format, currently =DOT=,
       =HTML_TABLE= or =SVG=.

For the output formats we require the following things:
    1. Tables and 2D canvas drawings are very different output formats.
       The former will receive a list (i.e., the rows) of lists (i.e., the
       columns) of vertex descriptions, whereas the latter will receive a
       list of pairs of vertex descriptions and vertex coordinates.

@author Wouter Beek
@version 2012/12-2013/04
*/

:- use_module(generics(meta_ext)).
:- use_module(graph_theory(graph_generic)).
:- use_module(math(math_ext)).
:- use_module(standards(graphviz)).
:- use_module(html(html)).
:- use_module(standards(markup)).
:- use_module(standards(graphviz)).
:- use_module(standards(standards)).
:- use_module(svg(svg)).



% SETTINGS %

default_border(size(2, [0.5, 0.5])).
default_surface(size(2, [10.0, 10.0])).
default_vertice_radius(0.1).



% EXPORTING %

% BASIC PROCEDURES: CREATE (INPUT -> GRAPH) & WRITE (GRAPH -> OUTPUT) %

%! export_graph(+Options:list(nvpair), +Graph, -Export) is det.
% Creates a graph in the format specified using the =output_format= option.
%
% @arg Options A list of name-value pairs.
%        The following options are supported:
%        1. =|colorscheme(oneof([none,svg,x11]))|= The colorscheme for the
%           colors assigned to vertices and edges.
%           Supported for: GraphViz, HTML_TABLE.
%           Default: =svg=.
%        2. =|edge_labels(oneof([all,none,replace])|= Whether edge labels are
%           displayed, not displayed, or replaced by alternative labels.
%           Supported for: RDF, UGRAPH.
%           Default: =none=.
%        3. =|in(oneof([rdf,ugraph]))|= The input / graph language.
%        4. =language(Language:atom)= The atomic tag of the language that is
%           preferred for vertex naming.
%           Defaults to =en=.
%        5. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Default: =collapse=.
%           Supported for: RDF.
%        6. =|out(oneof([graphviz,html_table,svg]))|= The language
%           of the output / visualized graph structure.
%           No default. Required.
%        7. =|rdf_list(onef([concise,full]))|= Whether vertices that are part
%           of an RDF list should be included or not.
%           Default: =full=.
%           Supported for: RDF.
%        8. =|vertex(oneof([rdf_node,rdf_term])|=
%           Value =rdf_node= means that only subject and object terms are
%           considered as vertices.
%           Value =rdf_term= means that subject, predicate and object terms
%           are considered as vertices.
%           Default: =rdf_node=.
%           Supported for: RDF.
%        9. =|vertex_coordinate(oneof([none,circular_vertice_coordinate,lookup_vertice_coordinate]))|=
%           The algorithm used for determining the vertex coordinates.
%           Default: =none=.
%           Supported for: SVG.
% @arg Graph
% @arg Export

export_graph(Options0, Graph, GraphElement):-
  % Set graph name and input format.
  graph_format(Graph, InputFormat),
  merge_options([graph(Graph), in(InputFormat)], Options0, Options1),

  % Set edges.
  (
    option(edges(_Edges), Options1)
  ->
    Options2 = Options1
  ;
    edges1(Options1, Edges),
    merge_options([edges(Edges)], Options1, Options2)
  ),

  % Set vertices.
  (
    option(vertices(_Vertices), Options2)
  ->
    Options3 = Options2
  ;
    vertices1(Options2, Vertices),
    merge_options([vertices(Vertices)], Options2, Options3)
  ),

  % Let's go...
  export_triples0(Options3, GraphElement).

export_triples(Options, Triples, GraphElement):-
  triples_to_vertices(Triples, Vertices),
  triples_to_edges(Triples, Edges),
  export_triples0(
    [edges(Edges), vertices(Vertices) | Options],
    GraphElement
  ).

export_triples0(Options, GraphElement):-
  option(edges(Edges), Options),
  option(vertices(Vertices), Options),

  % Vertices need not be religitimate identifiers in the output format
  % language.
  flag(vertice_id, _OldVertexID, 0),
  setoff(
    Vertex/VertexID,
    (
      member(Vertex, Vertices),
      flag(vertice_id, VertexID, VertexID + 1)
    ),
    VertexPairs
  ),
  option(out(OutputFormat), Options),

  % Vertex elements.
  findall(
    VertexElement,
    (
      member(Vertex/VertexID, VertexPairs),
      create_vertex(OutputFormat, Options, Vertex/VertexID, VertexElement)
    ),
    VertexElements
  ),
  % Edge elements
  setoff(
    EdgeElement,
    (
      member(FromVertex-ToVertex, Edges),
      memberchk(FromVertex/FromVertexID, VertexPairs),
      memberchk(ToVertex/ToVertexID, VertexPairs),
      create_edge(
        OutputFormat,
        Options,
        FromVertex/FromVertexID,
        ToVertex/ToVertexID,
        EdgeElement
      )
    ),
    EdgeElements
  ),

  % Graph element
  create_graph(
    OutputFormat,
    Options,
    VertexElements,
    EdgeElements,
    GraphElement
  ).

%! export_vertex(
%!   +Options:list(nvpair),
%!   +Vertex:vertex,
%!   -GraphElement:element
%! ) is det.
% Exports the given vertex to the interchangeable graph element format.
%
% @arg Options A list name-value pairs. See export_graph/3 for the list of
%        options. In addition, the following options are defined:

export_vertex(O1, Vertex, GraphElement):-
  % Depth
  option(depth(Depth), O1, 1),
  % Edges and vertices
  merge_options([directed(false)], O1, O2),
  depth(O2, Vertex, Depth, Vertices, Edges),
  merge_options([edges(Edges), vertices(Vertices)], O2, O3),
  export_triples0(O3, GraphElement).



% EXPORT TO DOM %

graph_element_to_dom(Options, GraphElement, SVG):-
  % The stream for the GraphViz data.
  %new_memory_file(Handle),
  %open_memory_file(Handle, write, Stream),
  absolute_file_name(
    personal(from),
    FromFile,
    [access(write), file_type(graphviz)]
  ),
  open(FromFile, write, FromStream, []),
  (
    option(out(graphviz), Options)
  ->
    stream_graphviz(FromStream, GraphElement),
    graphviz_to_svg(FromFile, sfdp, SVG)
  ),
  close(FromStream),
  delete_file(FromFile).
  %free_memory_file(Handle).

%! graph_to_dom(
%!   +Options:list(nvpair),
%!   +Graph:atom,
%!   -DOM:list
%! ) is det.
% Writes the graph with the given name to the given file name.
% The files are located in =debug(rdf_graph)=. Since this method is
% only used for debugging purposes, this will suffice for the moment.
%
% @arg Options A list of name-value pairs. See export_graph/3 for the list
%        of options.
% @arg Graph The atomic name of a graph.
% @arg DOM

graph_to_dom(Options, Graph, DOM):-
  export_graph(Options, Graph, GraphElement),
  graph_element_to_dom(Options, GraphElement, DOM).

triples_to_dom(Options, Triples, DOM):-
  export_triples(Options, Triples, GraphElement),
  graph_element_to_dom(Options, GraphElement, DOM).

triples_to_edges(Triples, Edges):-
  setoff(
    FromVertex-ToVertex,
    member(rdf(FromVertex, _, ToVertex), Triples),
    Edges
  ).

triples_to_vertices(Triples, Vertices):-
  setoff(
    Vertex,
    (
      member(rdf(Vertex1, _, Vertex2), Triples),
      (Vertex = Vertex1 ; Vertex = Vertex2)
    ),
    Vertices
  ).

%! vertex_to_dom(
%!   +Options:list(nvpair),
%!   +Vertex:vertex,
%!   -DOM:list
%! ) is det.
% Exports the given vertex to the interchangeable graph element format.
%
% @arg Options A list of name-value pairs. See export_vertex/3 for the list
%        of options.
% @arg Vertex
% @arg DOM

vertex_to_dom(Options, Vertex, DOM):-
  export_vertex(Options, Vertex, GraphElement),
  graph_element_to_dom(Options, GraphElement, DOM).



% VERTICE COORDINATES %

circular_vertice_coordinate(Options, Vertex, coordinate(2, [X, Y])):-
  % Surface
  default_surface(DefaultSurface),
  option(surface(size(2, [Width, Height])), Options, DefaultSurface),

  % Borders
  default_border(DefaultBorder),
  option(border(size(2, [X_Border, Y_Border])), Options, DefaultBorder),

  % Graph radius.
  GraphXDiameter is Width - 2 * X_Border,
  GraphYDiameter is Height - 2 * Y_Border,
  GraphDiameter is max(GraphXDiameter, GraphYDiameter),
  GraphRadius is GraphDiameter / 2,

  % Vertex index
  option(vertices(Vertices), Options),
  nth0chk(I, Vertices, Vertex),

  % Angle
  length(Vertices, NumberOfVertices),
  % Specifically cater for the case in which there are no vertices.
  (
    NumberOfVertices == 0
  ->
    AnglePerVertice is 2 * pi
  ;
    AnglePerVertice is 2 * pi / NumberOfVertices
  ),

  % X-coordinate.
  X is X_Border + GraphRadius + GraphRadius * cos(I * AnglePerVertice),
  % Y-coordinate.
  Y is Y_Border + GraphRadius + GraphRadius * sin(I * AnglePerVertice).

lookup_vertice_coordinates(Options, Vertex, Coordinate):-
  option(vertice_coordinates(VerticeCoordinates), Options),
  memberchk(vertex_coordinate(Vertex, Coordinate), VerticeCoordinates).



% OUTPUT FORMAT LAYER %

%! create_edge(
%!   +OutputFormat:oneof([graphviz,svg]),
%!   +Options:list(nvpair),
%!   +FromVertexPair:pair(vertex,integer)
%!   +ToVertexPair:pair(vertex,integer)
%!   -EdgeElement:element
%! ) is det.

create_edge(
  graphviz,
  Options,
  FromVertex0/FromVertexID0,
  ToVertex0/ToVertexID0,
  edge(FromVertexID0, ToVertexID0, Attributes)
):-
  (
    FromVertex = FromVertex0,
    ToVertex = ToVertex0,
    Direction = forward
  ;
    option(directed(false), Options, true),
    FromVertex = ToVertex0,
    ToVertex = FromVertex0,
    Direction = back
  ),

  % Label
  option(edge_labels(DisplayEdgeLabels), Options, none),
  (
    DisplayEdgeLabels == none
  ->
    Attributes0 = []
  ;
    edge_naming(Options, FromVertex-ToVertex, Label),
    Attributes0 = [label(Label)]
  ),

  % Style and arrow head.
  edge_styling(Options, FromVertex-ToVertex, Style/ArrowHead),

  append([arrowhead(ArrowHead), dir(Direction), style(Style)], Attributes0, Attributes),
  parse_attributes_graphviz(edge, Attributes).

create_edge(
  html_table,
  Options,
  FromVertex/_FromVertexID,
  ToVertex/_ToVertexID,
  element(tr, [], Cells)
):-
  vertex_naming(Options, FromVertex, FromVertexLabel),
  vertex_coloring(Options, FromVertex, FromColor),
  format(atom(FromColorStyle), 'color:~w', [FromColor]),
  FromVertexCell = element(td, [style=FromColorStyle], [FromVertexLabel]),

  vertex_naming(Options, ToVertex, ToVertexLabel),
  vertex_coloring(Options, ToVertex, ToColor),
  format(atom(ToColorStyle), 'color:~w', [ToColor]),
  ToVertexCell = element(td, [style=ToColorStyle], [ToVertexLabel]),

  option(edge_labels(DisplayEdgeLabels), Options, none),
  (
    DisplayEdgeLabels == none
  ->
    Cells = [FromVertexCell, ToVertexCell]
  ;
    edge_naming(Options, FromVertex-ToVertex, EdgeLabel),
    edge_coloring(Options, FromVertex-ToVertex, EdgeColor),
    format(atom(EdgeColorStyle), 'color:~w', [EdgeColor]),
    EdgeCell = element(td, [style=EdgeColorStyle], [EdgeLabel]),
    Cells = [FromVertexCell, EdgeCell, ToVertexCell]
  ).

create_edge(
  svg,
  Options,
  FromVertex/_FromVertexID,
  ToVertex/_ToVertexID,
  element(line, ParsedEdgeAttributes, EdgeContent)
):-
  % Label
  option(edge_labels(DisplayEdgeLabels), Options, none),
  (
    DisplayEdgeLabels == none
  ->
    EdgeContent = []
  ;
    edge_naming(Options, FromVertex-ToVertex, EdgeLabel),
    EdgeContent = [element(title, [], [EdgeLabel])]
  ),

  % Coords
  option(vertex_coordinate(VC_Pred), Options),
  call(VC_Pred, Options, FromVertex, coordinate(2, [X1, Y1])),
  call(VC_Pred, Options, ToVertex, coordinate(2, [X2, Y2])),
  format_number(X1, cm, X1_cm),
  format_number(Y1, cm, Y1_cm),
  format_number(X2, cm, X2_cm),
  format_number(Y2, cm, Y2_cm),

  EdgeAttributes =
    [x1(X1_cm), y1(Y1_cm), x2(X2_cm), y2(Y2_cm), stroke(black)],
  parse_attributes_svg(line, EdgeAttributes, ParsedEdgeAttributes).

%! create_graph(
%!   +OutputFormat:oneof([graphviz,svg]),
%!   +Options:list(nvpair),
%!   +VertexElements:list(element),
%!   +EdgeElements:list(element),
%!   -GraphElement:element
%! ) is det.

create_graph(
  graphviz,
  Options,
  VertexElements,
  EdgeElements,
  graph(VertexElements, EdgeElements, GraphAttributes)
):-
  % Label
  graph_naming(Options, GraphLabel),

  option(colorscheme(Colorscheme), Options, svg),
  GraphAttributes =
    [
      charset('UTF-8'),
      colorscheme(Colorscheme),
      fontsize(11.0),
      label(GraphLabel),
      overlap(false)
    ],
  parse_attributes_graphviz(graph, GraphAttributes).

create_graph(
  html_table,
  Options,
  _VertexElements,
  EdgeElements,
  element(table, ParsedGraphAttributes, [HeaderRow | EdgeElements])
):-
  % Header row
  option(edge_labels(DisplayEdgeLabels), Options, none),
  (
    DisplayEdgeLabels == none
  ->
    HeaderRow = element(tr, [], [
                  element(th, [], ['From']),
                  element(th, [], ['To'])])
  ;
    HeaderRow = element(tr, [], [
                  element(th, [], ['From']),
                  element(th, [], ['Edge']),
                  element(th, [], ['To'])])
  ),
  GraphAttributes = [border(1)],
  parse_attributes_html(table, GraphAttributes, ParsedGraphAttributes).

create_graph(svg, Options, VertexElements, EdgeElements, SVG_Root):-
  option(vertex_coordinate(circular_vertice_coordinate), Options),
  !,
  default_surface(DefaultSurface),
  option(surface(Surface), Options, DefaultSurface),
  svg_head(Surface, SVG_Head),

  % Border
  default_border(DefaultBorder),
  option(border(size(2, [X_Border, Y_Border])), Options, DefaultBorder),

  % Graph origin
  Surface = size(2, [Width, Height]),
  X is Width / 2,
  Y is Height / 2,
  format_number(X, cm, X_cm),
  format_number(Y, cm, Y_cm),

  % Graph radius
  GraphXDiameter is Width - 2 * X_Border,
  GraphYDiameter is Height - 2 * Y_Border,
  GraphDiameter is max(GraphXDiameter, GraphYDiameter),
  GraphRadius is GraphDiameter / 2,
  format_number(GraphRadius, cm, GraphRadius_cm),

  % Graph attributes
  GraphAttributes =
    [
      cx(X_cm),
      cy(Y_cm),
      fill(none),
      r(GraphRadius_cm),
      stroke(black),
      'stroke-width'='0.05cm'
    ],
  parse_attributes_svg(circle, GraphAttributes, ParsedGraphAttributes),
  GraphElement = element(circle, ParsedGraphAttributes, []),
  append([GraphElement | EdgeElements], VertexElements, Elements),

  root_element(svg, SVG_Head, Elements, SVG_Root).

create_graph(
  svg,
  Options,
  VertexElements,
  EdgeElements,
  SVG_Root
):-
  default_surface(DefaultSurface),
  option(surface(Surface), Options, DefaultSurface),
  svg_head(Surface, SVG_Head),
  append(VertexElements, EdgeElements, Elements),
  root_element(svg, SVG_Head, Elements, SVG_Root).

%! create_vertex(
%!   +OutputFormat:setof([graphviz,svg]),
%!   +Options:list(nvpair),
%!   +VerticePair:pair(vertex,integer),
%!   -VertexElement:element
%! ) is det.

create_vertex(graphviz, Options, Vertex/ID, node(ID, Attributes)):-
  % Label
  vertex_naming(Options, Vertex, Name),

  % Color
  vertex_coloring(Options, Vertex, Color),

  % Shape
  vertex_shaping(Options, Vertex, ShapeAttributes),

  % Picture
  (
    vertex_picturing(Options, Vertex, Image)
  ->
    ImageAttributes = [image(Image)]
  ;
    ImageAttributes = []
  ),

  append(ShapeAttributes, ImageAttributes, OtherAttributes),
  Attributes = [color(Color), label(Name) | OtherAttributes],
  parse_attributes_graphviz(node, Attributes).

create_vertex(
  svg,
  Options,
  Vertex/_VertexID,
  element(circle, ParsedVerticeAttributes, VerticeContent)
):-
  % Label
  vertex_naming(Options, Vertex, VerticeName),

  % Color
  vertex_coloring(Options, Vertex, VertexColor),

  % Coord
  option(vertex_coordinate(VC_Pred), Options),
  call(VC_Pred, Options, Vertex, coordinate(2, [X0, Y0])),
  format_number(X0, cm, X0_cm),
  format_number(Y0, cm, Y0_cm),

  % Radius
  default_vertice_radius(DefaultVerticeRadius),
  option(vertice_radius(VerticeRadius), Options, DefaultVerticeRadius),
  format_number(VerticeRadius, cm, VerticeRadius_cm),

  VerticeAttributes =
    [cx(X0_cm), cy(Y0_cm), r(VerticeRadius_cm), stroke(VertexColor)],
  parse_attributes_svg(circle, VerticeAttributes, ParsedVerticeAttributes),

  VerticeContent = [element(title, [], [VerticeName])].

