:- module(
  rdf_export,
  [
    rdf_edge_coloring/3, % +Options:list(nvpair)
                         % +Edge:edge
                         % -Color:color
    rdf_edge_naming/3, % +Options:list(nvpair)
                       % +Edge:edge
                       % -Name:atom
    rdf_edge_styling/3, % +Options:list(nvpair)
                        % +Edge:edge
                        % -Style:pair(oneof([bold,dashed,dotted,solid]),atom)
    rdf_graph_naming/2, % +Options:list(nvpair)
                        % -Name:atom
    rdf_register_class_color/3, % +Graph:atom
                                % +Class:class
                                % +Color:atom
    rdf_register_namespace_color/3, % +Graph:graph
                                    % +Namespace:atom
                                    % +Color:atom
    rdf_schema/2, % +Graph:atom
                  % -Triples:list(rdf_triple)
    rdf_vertex_coloring/3, % +Options:list(nvpair)
                           % +Vertex:vertex
                           % -Color:atom
    rdf_vertex_naming/3, % +Options:list(nvpair)
                         % +Vertex:vertex
                         % -Name:atom
    rdf_vertex_picturing/3, % +Options:list(nvpair)
                            % +Vertex:vertex
                            % -Image:atom
    rdf_vertex_shaping/3 % +Options:list(nvpair)
                         % +Vertex:vertex
                         % -Name:atom
  ]
).

/** <module> RDF export

Predicates for exporting RDF.

---+ Edge naming

Specific edge labels can be hidden by adding clauses to
rdf_edge_label_replace/2.

---+ Edge styling

---+ Vertex coloring

The procedure for determining the color of a vertex:
    1. Look whether the =colorscheme= option is not set to =none=.
    2. See whether the vertex is an individual of a colored class.
    3. See whether the vertex belongs to a colored namespace.
    4. If at least one vertex is not colored by class or namespace, then all
       namespaces are (re)assigned colors.

---+ Vertex naming

@author Wouter Beek
@version 2013/01-2013/03
*/

:- use_module(pgc(atom_ext)).
:- use_module(pgc(list_ext)).
:- use_module(pgc(meta_ext)).
:- use_module(
  generic(print),
  [indent/2, lines/2 as print_lines, list/2 as print_list]
).
:- use_module(pgc(type_checking)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_datatype)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_graph_theory)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_read)).
:- use_module(standards(svg)).

:- dynamic(class_color(_Graph, _Class, _Color)).
:- dynamic(namespace_color(_Graph, _Namespace, _Color)).
:- dynamic(rdf_edge_style(_Predicate, _Style)).

:- rdf_meta(rdf_edge_label_replace(r,r)).
:- rdf_meta(rdf_edge_style(r,?)).
:- rdf_meta(rdf_register_class_color(+,r,+)).



%% colorize_namespaces(+Graph:atom, +ColorScheme:atom) is det.
% Uses colors from the given colorscheme to colorize the namespaces in the
% given graph.
%
% @tbd Throw exception for unknown colorscheme.

colorize_namespaces(Graph, _ColorScheme):-
  \+ rdf_graph(Graph),
  !,
  existence_error(atom, Graph).
colorize_namespaces(Graph, svg):-
  !,
  rdf_current_namespaces(Graph, Namespaces),
  length(Namespaces, N),
  N > 0,
  svg_colors(Colors),
  length(Colors, M),
  Delta is M // N,
  forall(
    nth1(I, Namespaces, Namespace),
    (
      % In case there are more namespaces than colors, Delta=1 and we use
      % the same color for all namespaces with index I mod M.
      J is (I * Delta) mod M,
      % J can be 0 becasue of the modulus function, so do not use nth1/3.
      nth0(J, Colors, Color),
      assert(namespace_color(Graph, Namespace, Color))
    )
  ).
colorize_namespaces(_Graph, ColorScheme):-
  existence_error(atom, ColorScheme).

rdf_edge_coloring(Options, _Edge, black):-
  option(colorscheme(none), Options),
  !.
rdf_edge_coloring(Options, FromVertex-ToVertex, Color):-
  rdf_vertex_coloring(Options, FromVertex, FromColor),
  rdf_vertex_coloring(Options, ToVertex, ToColor),
  !,
  % Notive that color must be uninstantiated in rdf_vertex_coloring/3.
  FromColor = ToColor,
  Color = FromColor.
rdf_edge_coloring(Options, FromVertex-ToVertex, Color):-
  option(graph(Graph), Options),
  rdf(FromVertex, Predicate, ToVertex, Graph),
  rdf_vertex_coloring(Options, Predicate, Color).

%% rdf_edge_naming(+Options:list(nvpair), +Edge:edge, -Name:atom) is det.
% Returns a name for the given edge.
%
% @param Options A list of name-value pairs. The following options are
%        supported:
%            1. =|edge_labels(oneof([all,replace]))|=

rdf_edge_naming(Options, FromVertex-ToVertex, Name):-
  option(in(rdf), Options),
  !,
  option(graph(Graph), Options, user),
  rdf(FromVertex, Predicate, ToVertex, Graph),
  % Some edge labels are not displayed.
  % This concerns RDF(S) terminology.
  (
    option(edge_labels(replace), Options),
    rdf_edge_label_replace(Predicate, Name)
  ->
    true
  ;
    rdf_vertex_naming(Options, Predicate, Name)
  ).

rdf_edge_label_replace(rdf:type,           '').
rdf_edge_label_replace(rdfs:label,         '').
rdf_edge_label_replace(rdfs:subClassOf,    '').
rdf_edge_label_replace(rdfs:subPropertyOf, '').

rdf_edge_style1(Predicate, Style):-
  rdf_edge_style(Predicate, Style).
rdf_edge_style1(_OtherPredicate, solid/normal).

% These values come from the GraphViz attributes =arrowhead= and =style=.
rdf_edge_style(rdf:type,           solid/empty).
rdf_edge_style(rdfs:label,         dotted/none).
rdf_edge_style(rdfs:subClassOf,    solid/box).
rdf_edge_style(rdfs:subPropertyOf, solid/diamond).

rdf_edge_styling(Options, FromVertex-ToVertex, Style):-
  option(graph(Graph), Options, user),
  rdf(FromVertex, Predicate, ToVertex, Graph),
  rdf_edge_style1(Predicate, Style),
  !.

%% rdf_graph_naming(+Options:list(nvpair), -Name:atom) is det.
% Returns a name for the given graph.
%
% @param Options A list of name-value paris.
%        1. =graph(Graph:atom)= The atomic name of a graph.
% @param Graph The atomic name of a graph.
% @param Name An atomic name for the given graph.

rdf_graph_naming(Options, GraphName):-
  option(graph(GraphName), Options).

rdf_register_class_color(Graph, Class, Color):-
  assertz(class_color(Graph, Class, Color)).

rdf_register_namespace_color(Graph, Namespace, Color):-
  assertz(namespace_color(Graph, Namespace, Color)).

rdf_schema(Graph, Triples):-
  setoff(
    Vertex,
    (
      (
        rdfs_individual_of(Vertex, rdfs:'Class')
      ;
        rdfs_individual_of(Vertex, rdf:'Property')
      ),
      rdf_export:rdf_vertex([graph(Graph)], Vertex)
    ),
    Vertices
  ),
  setoff(
    rdf(Subject, Predicate, Object),
    (
      member(Subject, Object, Vertices),
      rdf(Subject, Predicate, Object, Graph)
    ),
    Triples
  ).

%% rdf_vertex_color_by_namespace(
%%   +Graph:atom,
%%   +ColorScheme:atom,
%%   +Vertex,
%%   -Color:atom
%% ) is det.
% Returns the automatically assigned color name.
% The color names belong to the given colorscheme.
% The color assignments are based on the RDF node's namespace.
% Note that the same node may have different colors in different graphs.
%
% @param Graph The atomic name of a graph.
% @param ColorScheme The atomic name of a colorscheme. Currently supported:
%        1. =svg=
%        2. =x11=
% @param Vertex A resource.
% @param Color The atomic name of a color within the colorscheme.

rdf_vertex_color_by_namespace(Graph, _ColorScheme, Vertex, Color):-
  rdf_global_id(Namespace:_, Vertex),
  namespace_color(Graph, Namespace, Color),
  !.
rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color):-
  colorize_namespaces(Graph, ColorScheme),
  rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color).

%% rdf_vertex_coloring(
%%   +Options:list(nvpair),
%%   +Vertex:vertex,
%%   -Color:atom
%% ) is det.
% Returns a color name for the given vertex.
%
% @param Options A list of name-value pairs.
%        1. =colorscheme(ColorScheme:oneof([none,svg,x11]))= The atomic name
%           of the color scheme from which the color names are drawn.
%        2. =graph(Graph:atom)= The atomic name of a graph.
% @param Vertex A vertex.
% @param Color The atomic name of a color for the given vertex.

rdf_vertex_coloring(Options, _Vertex, black):-
  option(colorscheme(none), Options),
  !.
% Literals.
rdf_vertex_coloring(_Options, literal(lang(_Language, _Label)), blue):-
  !.
rdf_vertex_coloring(_Options, literal(type(_Datatype, _Value)), blue):-
  !.
% Individual or subclass of a color-registered class.
rdf_vertex_coloring(Options, Vertex, Color):-
  option(graph(Graph), Options),
  (
    rdfs_individual_of(Vertex, Class)
  ;
    rdfs_subclass_of(Vertex, Class)
  ),
  class_color(Graph, Class, Color),
  !.
% Resource colored based on its namespace.
rdf_vertex_coloring(Options, Vertex, Color):-
  option(colorscheme(ColorScheme), Options, svg),
  option(graph(Graph), Options),
  (
    % URI resources with registered namespace/prefix.
    rdf_global_id(_:_, Vertex)
  ->
    rdf_vertex_color_by_namespace(Graph, ColorScheme, Vertex, Color)
  ;
    % URI resources with unregistered namespace/prefix.
    uri(Vertex)
  ->
    Color = red
  ;
    % Non-URI resources, e.g., literals.
    Color = purple
  ).

vertex_label(Options, Vertex, Label):-
  option(language(Language), Options),
  % We prefer labels with the given language code.
  rdfs_label(Vertex, Language, Label),
  !.
vertex_label(_Options, Vertex, Label):-
  rdfs_label(Vertex, _OtherLanguage, Label).

%% rdf_vertex_naming(+Options:list(nvpair), +Vertex, -VertexName:atom) is det.
% Returns a display name for the given RDF graph vertex.
%
% @param Options A list of name-value pairs.
%        1. =graph(Graph:atom)=
%        2. =language(Language:atom)= The atomic tag of the language that is
%           preferred for vertex naming.
%           Defaults to =en=.
%        3. =|literals(oneof([collapse,hide,labels_only,show]))|=
%           Whether or not literals are allowed as vertices in the =Edge=.
%           Default: =collapse=.
% @param Vertex A vertex.
% @param VertexName An atomic name for the vertex.

% The vertex is a list.
% This comes before all this others, since all the others could be members of
% a list.
rdf_vertex_naming(Options, Vertices, Name):-
  is_list(Vertices),
  !,
  maplist(rdf_vertex_naming(Options), Vertices, Names),
  print_list(atom(Name), Names).
rdf_vertex_naming(Options, Vertex, Name):-
  rdfs_individual_of(Vertex, rdf:'List'),
  !,
  rdf_list([recursive(true)], Vertex, Vertices),
  maplist(rdf_vertex_naming(Options), Vertices, Names),
  print_list(atom(Name), Names).
% The vertex is itself a label, with some language tag.
rdf_vertex_naming(_Options, literal(lang(Language, Name0)), Name):-
  !,
  format(atom(Name), '~w^~w', [Name0, Language]).
% The vertex is a literal that is of an XML Schema datatype.
rdf_vertex_naming(_Options, literal(type(Datatype, CanonicalValue)), Name):-
  rdf_datatype(DatatypeName, _LexicalValue, Datatype, CanonicalValue),
  !,
  format(atom(Name), '~w^~w', [CanonicalValue, DatatypeName]).
% The vertex has a label and is set to displaying labels as the only literals.
% Note that =|labels_only|= is only a preferred option. If there is no label
% then we use a name that is based on the URI of the vertex.
rdf_vertex_naming(Options, Vertex, Name):-
  option(literals(labels_only), Options),
  vertex_label(Options, Vertex, Name),
  !.
% The vertex is set to collate all literals that (directly) relate to it.
% Only do this when there is at least one literal.
rdf_vertex_naming(Options, Vertex, Name):-
  option(graph(Graph), Options),
  % We first dispaly the name.
  (
    % Use the label as name, if any (prefering the given language).
    vertex_label(Options, Vertex, Name1)
  ->
    true
  ;
    % Or use the namespace (if non-graph) name plus the local name.
    rdf_resource_to_namespace(Vertex, Namespace, Name0),
    !,
    (
      Namespace == Graph
    ->
      % If namespace and graph name are the same, then the namespace is not
      % displayed as part of the vertex name.
      Name1 = Name0
    ;
      format(atom(Name1), '~w:~w', [Namespace, Name0])
    )
  ;
    % If all else fails...
    term_atom(Vertex, Name1)
  ),
  % Then come the literals, but only if these are set to be collapsed into
  % the (directly) related vertex.
  (
    option(literals(collapse), Options, collapse)
  ->
    findall(
      Literal1,
      (
        rdf(Vertex, _Predicate, Literal, Graph),
        rdf_is_literal(Literal),
        rdf_vertex_naming(Options, Literal, Literal1)
      ),
      Names
    ),
    % When there are no literals, then use the name. Otherwise, use the
    % literals.
    (
      Names == []
    ->
      Names1 = [Name1]
    ;
      Names1 = Names
    )
  ;
    Names1 = [Name1]
  ),
  % Done!
  print_lines(atom(Name), Names1).

rdf_vertex_picturing(Options, Vertex, Picture):-
  option(graph(Graph), Options),
  % Only display the first picutre that is related to this vertex.
  rdf_datatype(Vertex, _Predicate, image, Picture, Graph).

% Non-resource vertex (e.g. literals).
rdf_vertex_shaping(_Options, literal(_), [peripheries(0), shape(plaintext)]):-
  !.
% Class vertex.
rdf_vertex_shaping(_Options, Vertex, [peripheries(2), shape(octagon)]):-
  rdfs_individual_of(Vertex, rdfs:'Class'),
  !.
% Property vertex.
rdf_vertex_shaping(_Options, Vertex, [peripheries(1), shape(hexagon)]):-
  rdfs_individual_of(Vertex, rdf:'Property'),
  !.
% Non-class and non-property resource vertex.
rdf_vertex_shaping(_Options, Vertex, [peripheries(1), shape(ellipse)]):-
  rdfs_individual_of(Vertex, rdfs:'Resource'),
  !.
% Blank nodes.
rdf_vertex_shaping(_Options, Vertex, [peripheries(1), shape(circle)]):-
  rdf_is_bnode(Vertex),
  !.
% Debugging.
rdf_vertex_shaping(_Options, _Vertex, [peripheries(1), shape(ellipse)]):-
  %gtrace, % DEB
  true.

