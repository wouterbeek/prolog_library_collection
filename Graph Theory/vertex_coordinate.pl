:- module(
  vertex_coordinate,
  [
    random_vertice_coordinates/2, % +Options:list(nvpair)
                                  % -VerticeCoordinates:list(vertex_coordinate)
    vertice_coordinates_to_size/3, % +Options:list(nvpair)
                                   % +VerticeCoordinates:list(vertex_coordinate)
                                   % -Size:size
    vertice_coordinates_table/2, % +VerticeCoordinates:list(vertex_coordinate)
                                 % -MarkupElement:element
    vertice_coordinates_web/3 % +Graph:graph
                              % +VerticeCoordinates:list(vertex_coordinate)
                              % -MarkupElement:element
  ]
).

/** <module> Vertex coordinate

Methods for processing and geenrating vertex coordinates.

These are used for visualizing graphs and for calculating the vertex
positions in iterations of spring embedding.

@author Wouter Beek
@version 2013/01
*/

:- use_module(pgc(list_ext)).
:- use_module(graph_theory(graph_export)).
:- use_module(graph_theory(graph_generic)).
:- use_module(math(math_ext)).
:- use_module(standards(html)).



%% random_vertice_coordinates(
%%   +Options:list(nvpair),
%%   -VerticeCoordinates:list(vertex_coordinate)
%% ) is det.
%
% @param Options A list of name-value pairs.
%        1. =graph(Graph:graph)=
%        2. =surface(Size:size)=
% @param VerticeCoordinates A list of vertex coordinates.

random_vertice_coordinates(Options, RandomVerticeCoordinates):-
  graph_export:default_surface(DefaultSurface),
  option(surface(Surface), Options, DefaultSurface),
  vertices1(Options, Vertices),
  findall(
    vertex_coordinate(Vertex, RandomCoordinate),
    (
      member(Vertex, Vertices),
      random_coordinate(Surface, RandomCoordinate)
    ),
    RandomVerticeCoordinates
  ).

%% vertice_coordinates_to_size(
%%   +Options:list(nvpair),
%%   +VCs:list(vertex_coordinate),
%%   -Size:size
%% ) is det.
% Returns the size structure that is big enough to display the given vertex
% coordinates. This means that the size has the following properties:
%     1. Its dimension is the same as the dimension of each vertex coordinate
%        (we assume the vertex coordinates are all in the same dimension).
%     2. Each of its subsizes (for each dimension one) starts at 0.
%     3. Each of its subsizes (for each dimension one) can represent every
%        of the given vertex coordinates.
%     4. Each of its subsizes (for each dimension one) has a border on both
%        sides of the spectrum. The border is either given or of the default
%        size.

vertice_coordinates_to_size(Options, VCs, size(Dimension, Limits)):-
  % Retrieve the dimension of the vertex coordinates, so that the default
  % borders can be generated.
  memberchk(vertex_coordinate(_, coordinate(Dimension, _)), VCs),
  repeating_list(0.5, Dimension, DefaultBorders),
  option(
    border(size(Dimension, Borders)),
    Options,
    size(Dimension, DefaultBorders)
  ),

  % For all indices =I=, find the maximum coordinate value for that index.
  findall(
    MaxI,
    (
      % We also use the borders for walking through all dimension used in VCs.
      nth0(I, Borders, Border),
      findall(
        Arg,
        (
          member(vertex_coordinate(_, coordinate(Dimension, Args)), VCs),
          nth0(I, Args, Arg)
        ),
        Args
      ),
      max_list(Args, MaxCVI),
      MaxI is MaxCVI + (2 * Border)
    ),
    Limits
  ).

%% vertice_coordinates_table(
%%   +VerticeCoordinates:list(vertex_coordinate),
%%   -MarkupElement:element
%% ) is det.
% Returns the markup for a table showing vertex coordinates.

vertice_coordinates_table(VerticeCoordinates, MarkupElement):-
  % Generate the header row for the table.
  memberchk(
    vertex_coordinate(_Vertex, coordinate(MaxDimension, _Coordinates)),
    VerticeCoordinates
  ),
  MaxDimension0 is MaxDimension - 1,
  findall(
    DimensionName,
    (
      between(0, MaxDimension0, Dimension),
      format(atom(DimensionName), 'Dimension ~w', [Dimension])
    ),
    DimensionNames
  ),
  % Generate the data rows for the table.
  findall(
    [Vertex | Coordinates],
    member(
      vertex_coordinate(Vertex, coordinate(_Dimension, Coordinates)),
      VerticeCoordinates
    ),
    Rows
  ),
  list_to_table(
    [header(true)],
    [['Vertex' | DimensionNames] | Rows],
    MarkupElement
  ).

%% vertice_coordinates_web(
%%   +Graph:graph,
%%   +VerticeCoordinates:list(vertex_coordinate),
%%   -MarkupElement:element
%% ) is det.
% Returns the markup for the given graph and vertex coordinates.

vertice_coordinates_web(Graph, VerticeCoordinates, MarkupElement):-
  export_graph(
    [
      out(svg),
      vertex_coordinate(lookup_vertice_coordinates),
      vertice_coordinates(VerticeCoordinates)
    ],
    Graph,
    MarkupElement
  ).

