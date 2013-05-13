:- module(
  graphviz,
  [
% FILE CONVERSION
    convert_graphviz/4, % +FromFile
                        % +Method:onef([dot,sfdp])
                        % +ToFileType:oneof([jpeg,pdf,svg,xdot])
                        % ?ToFile
    graphviz_to_svg/3, % +FromFile:stream
                       % +Method:onef([dot,sfdp])
                       % -SVG:list

% PARSING
    parse_attributes_graphviz/2, % +Context:oneof([edge,graph,node])
                                 % +Attributes:list(nvpair)

% STREAMING
    stream_graphviz/2 % +Stream:stream
                      % +Graph:element
  ]
).

/** <module> GraphViz

Methods for writing to GraphViz.

In GraphViz vertices are called 'nodes'.

# Datatypes

This module uses the following non-standard datatypes.

## edge

A compound term of the form

==
edge(FromVertexID, ToVertexID, EdgeAttributes)
==

representing a GraphViz edge.

# graph

A compound term of the form

==
graph(Vertices, Edges, GraphAttributes)
==

representing a GraphViz graph.

## vertex

A compound term of the form

==
vertex(VertexID, VerticeAttributes)
==

representing a GraphViz vertex.

# nvpair

A compound term of the form

==
Name(Value)
==

representing a name-value pair.

@author Wouter Beek
@version 2011-2013/04
*/

:- use_module(generics(db_ext)).
:- use_module(generics(exception_handling)).
:- use_module(generics(file_ext)).
:- use_module(generics(os_ext)).
:- use_module(generics(print_ext)).
:- use_module(generics(typecheck)).
:- use_module(library(process)).
:- use_module(standards(brewer)).
:- use_module(standards(c)).
:- use_module(svg(svg)).
:- use_module(standards(x11)).

:- db_add_novel(user:prolog_file_type(dot,  graphviz       )).
:- db_add_novel(user:prolog_file_type(jpeg, jpeg           )).
:- db_add_novel(user:prolog_file_type(jpeg, graphviz_output)).
:- db_add_novel(user:prolog_file_type(jpg,  jpeg           )).
:- db_add_novel(user:prolog_file_type(jpg,  graphviz_output)).
:- db_add_novel(user:prolog_file_type(pdf,  pdf            )).
:- db_add_novel(user:prolog_file_type(pdf,  graphviz_output)).
:- db_add_novel(user:prolog_file_type(svg,  graphviz_output)).
:- db_add_novel(user:prolog_file_type(svg,  svg            )).
:- db_add_novel(user:prolog_file_type(xdot, graphviz_output)).
:- db_add_novel(user:prolog_file_type(xdot, xdot           )).



% ATTRIBUTES %

arrow_type(box).
arrow_type(crow).
arrow_type(diamond).
arrow_type(dot).
arrow_type(ediamond).
arrow_type(empty).
arrow_type(halfopen).
arrow_type(inv).
arrow_type(invdot).
arrow_type(invempty).
arrow_type(invodot).
arrow_type(none).
arrow_type(normal).
arrow_type(obox).
arrow_type(odiamond).
arrow_type(odot).
arrow_type(open).
arrow_type(tee).
arrow_type(vee).

%! attribute(
%!   ?Name:atom,
%!   ?Type:term,
%!   ?Contexts:list(oneof([edge,graph,node])),
%!   ?Default:term
%! ) is nondet.
% Registered GraphViz attributes.
%
% @arg Name The atomic name of a GraphViz attribute.
% @arg Type The type of the values for this attribute.
% @arg Contexts A list representing the elements for which the attribute can
%        be specified. Any non-empty combination of the following values:
%        1. =edge=, the attribute applies to edges.
%        2. =graph=, the attribute applies to graphs.
%        2. =node=, the attribute applies to vertices.
% @arg Default The default value for the attribute.

attribute(arrowhead, oneof(ArrowTypes), [edge], normal):-
  findall(ArrowType, arrow_type(ArrowType), ArrowTypes).

% The character encoding used to interpret text label input.
% Note that iso-8859-1 and Latin1 denote the same character encoding.
attribute(
  charset,
  oneof(['iso-8859-1','Latin1','UTF-8']),
  [graph],
  'UTF-8'
).

% Color schemes from which values of the =color= attribute have to be drawn.
attribute(colorscheme, oneof([svg,x11]), [edge,graph,node], svg).

attribute(dir, oneof([back,both,forward,none]), [edge], forward).

attribute(image, atom, [node], '').

% The font size that is used for text.
attribute(fontsize, double, [graph], 1.0).

attribute(label, lblString, [edge,graph,node], '').

% Whether and how vertices are allowed to overlap.
% Boolean =false= is the same as =voronoi=.
% =scalexy= means that x and y are scaled separately.
attribute(
  overlap,
  or([boolean, oneof([compress,orthoxy,orthoyx,scalexy,voronoi,vpsc])]),
  [graph],
  true
).

attribute(peripheries, integer, [node], 1).

% Polygon-based shapes for GraphViz vertices.
attribute(
  shape,
  or([polygon_based_shape, record_based_shape, user_defined_shape]),
  [node],
  ellipse
).

% Styles for Graphviz edges.
attribute(style, oneof(Styles), [edge], ''):-
  findall(Style, style(edge, Style), Styles).

% Styles for Graphviz vertices.
attribute(style, oneof(Styles), [node], ''):-
  findall(Style, style(node, Style), Styles).

%! attribute(
%!   ?Name:atom,
%!   ?Type:term,
%!   ?Context:list(oneof([edge,graph,node])),
%!   +Attributess:list,
%!   ?Default:term
%! ) is nondet.
% Registered GraphViz attributes.
%
% @arg Name The atomic name of a GraphViz attribute.
% @arg Type The type of the values for this attribute.
% @arg Context A list representing the elements for which the attribute can
%        be specified. Any non-empty combination of the following values:
%        1. =edge=, the attribute applies to edges.
%        2. =graph=, the attribute applies to graphs.
%        2. =node=, the attribute applies to nodes.
% @arg Attributess A list of attribute-value pairs. Used for looking up the
%        interactions between multiple attributes.
% @arg Default The default value for the attribute.

attribute(color, oneof(Colors), [edge,graph,node], Attributes, black):-
  % The default =colorscheme= is either set by its own argument assertion,
  % or is assumed to be =x11=. We check whether the =colorscheme=
  % setting applies to the current context.
  (
    attribute(colorscheme, _Type, _Context, DefaultColorscheme),
    option(colorscheme(Colorscheme), Attributes, DefaultColorscheme)
  ->
    color_scheme_default_color(Colorscheme, DefaultColor)
  ;
    DefaultColor = black
  ),
  colorscheme_colors(Colorscheme, Colors).

attribute(Name, Type, Context, _Attrs, Default):-
  attribute(Name, Type, Context, Default).

color_scheme_default_color(_, black).



% FILE CONVERSION %

:- assert(user:prolog_file_type(dot,  dot     )).
:- assert(user:prolog_file_type(gv,   graphviz)).
:- assert(user:prolog_file_type(jpeg, jpeg    )).
:- assert(user:prolog_file_type(jpg,  jpeg    )).
:- assert(user:prolog_file_type(pdf,  pdf     )).
:- assert(user:prolog_file_type(svg,  svg     )).
:- assert(user:prolog_file_type(xdot, xdot    )).

%! convert_graphviz(
%!   +FromFile,
%!   +Method:oneof([dot,sfdp]),
%!   +ToFileType:oneof([jpeg,pdf,svg,xdot]),
%!   ?ToFile:atom
%! ) is det.
% Converts a GraphViz DOT file to an image file, using a specific
% visualization method.
%
% @arg FromFile
% @arg Method
% @arg ToFileType
% @arg ToFile

convert_graphviz(FromFile, Method, ToFileType, ToFile):-
  % Type checks.
  access_file(FromFile, read),
  gv_typecheck(oneof([dot,sfdp]), Method),
  prolog_file_type(ToExtension, ToFileType),
  prolog_file_type(ToExtension, graphviz_output),
  
  % The output file is either given or created.
  (
    var(ToFile)
  ->
    absolute_file_name(
      personal(export),
      ToFile,
      [access(write), file_type(ToFileType)]
    )
  ;
    is_absolute_file_name(ToFile),
    % The given output file must match a certain file extension.
    file_name_extension(_, ToFile, ToExtension)
  ),
  % Now that we have the output file we can prevent the
  % file type / file extension translation predicates from bakctracking.
  !,
  
  % Run the GraphViz conversion command in the shell.
  format(atom(OutputType), '-T~w', [ToExtension]),
  process_create(
    path(Method),
    [OutputType, FromFile, '-o', ToFile],
    [process(PID)]
  ),
  process_wait(PID, exit(ShellStatus)),
  rethrow(
    shell_status(ShellStatus),
    error(shell_error(FormalMessage), context(_Predicate, ContextMessage)),
    error(
      shell_error(FormalMessage),
      context(graphviz:dot/3, ContextMessage)
    )
  ).

graphviz_to_svg(FromFile, Method, SVG):-
  convert_graphviz(FromFile, Method, svg, ToFile),
  file_to_svg(ToFile, SVG),
  delete_file(ToFile).



% PARSING %

%! parse_attribute(
%!   +Context:oneof([edge,graph,node]),
%!   +Attributes:list,
%!   +Attribute:nvpair
%! ) is semidet.
% Succeeds if the given attribute term is a valid GraphViz name-value pair.
%
% @arg Context The atomic name of the element for which the attribute is
%        set. The same attribute may work differently for different elements.
%        1. =edge=
%        2. =graph=
%        3. =node=
% @arg Attributes A list of attribute-value pairs, used for resolving
%        interactions between the given =Attribute= and some other
%        attributes that were specified.
% @arg Attribute A name-value pair.

parse_attribute(Context, Attributes, Attribute):-
  Attribute =.. [Name, Value],
  attribute(Name, Type, Contexts, Attributes, _Default),
  % The same attribute may be defined for multiple elements. The meaning of
  % the attribute may be different for different elements.
  memberchk(Context, Contexts),
  !,
  gv_typecheck(Type, Value).

%! parse_attributes_graphviz(
%!   +Context:oneof([edge,graph,node]),
%!   +Attributes:list(nvpair)
%! ) is det.
% Parses a list of attributes.
%
% @arg Context The atomic name of the element to which the attributes apply.
% @arg Attributes A list of name-value pairs.

parse_attributes_graphviz(Context, Attributes):-
  maplist(parse_attribute(Context, Attributes), Attributes).



% TYPE CHECKING %

/* TODO
% html_label//0
% DCG for GraphViz HTML labels.

cell --> ['<td>'], label, ['</td>'].
cell --> ['<td>', '<img/>', '</td>'].

cells --> cell.
cells --> cells, cell.
cells --> cells, ['<vr/>'], cell.

html_label --> table.
html_label --> text.

label --> [Label], {atomic(Label)}.

row --> ['<tr>'], cells, ['</tr>'].

rows --> row.
rows --> rows, row.
rows --> rows, ['<hr/>'], row.

string --> [String], {atomic(String)}.

table --> table0.
table --> ['<font>'], table0, ['</font>'].

table0 --> ['<table>'], rows, ['</table>'].

text --> text_item.
text --> text, text_item.

text_item --> string.
text_item --> ['<br/>'].
text_item --> ['<font>'], text, ['</font>'].
text_item --> ['<i>'], text, ['</i>'].
text_item --> ['<b>'], text, ['</b>'].
text_item --> ['<u>'], text, ['</u>'].
text_item --> ['<sub>'], text, ['</sub>'].
text_item --> ['<sup>'], text, ['</sup>'].
*/

%! colorscheme_colors(?ColorScheme:atom, ?Colors:list(atom)) is det.
% The color names for each of the supported color schemes.
%
% @arg ColorScheme The atomic name of a GraphViz supported color scheme.
% @arg Colors A list of atomic names of colors in a specific color scheme.

colorscheme_colors(svg, Colors):-
  svg_colors(Colors),
  !.
colorscheme_colors(x11, Colors):-
  x11_colors(Colors),
  !.
colorscheme_colors(ColorScheme, Colors):-
  brewer_colors(ColorScheme, Colors).

%! shape(?Category:oneof([polygon]), ?Name:atom) is nondet.

shape(polygon, assembly).
shape(polygon, box).
shape(polygon, box3d).
shape(polygon, cds).
shape(polygon, circle).
shape(polygon, component).
shape(polygon, diamond).
shape(polygon, doublecircle).
shape(polygon, doubleoctagon).
shape(polygon, egg).
shape(polygon, ellipse).
shape(polygon, fivepoverhang).
shape(polygon, folder).
shape(polygon, hexagon).
shape(polygon, house).
shape(polygon, insulator).
shape(polygon, invhouse).
shape(polygon, invtrapezium).
shape(polygon, invtriangle).
shape(polygon, larrow).
shape(polygon, lpromoter).
shape(polygon, 'Mcircle').
shape(polygon, 'Mdiamond').
shape(polygon, 'Msquare').
shape(polygon, none).
shape(polygon, note).
shape(polygon, noverhang).
shape(polygon, octagon).
shape(polygon, oval).
shape(polygon, parallelogram).
shape(polygon, pentagon).
shape(polygon, plaintext).
shape(polygon, point).
shape(polygon, polygon).
shape(polygon, primersite).
shape(polygon, promoter).
shape(polygon, proteasesite).
shape(polygon, proteinstab).
shape(polygon, rarrow).
shape(polygon, rect).
shape(polygon, rectangle).
shape(polygon, restrictionsite).
shape(polygon, ribosite).
shape(polygon, rnastab).
shape(polygon, rpromoter).
shape(polygon, septagon).
shape(polygon, signature).
shape(polygon, square).
shape(polygon, tab).
shape(polygon, terminator).
shape(polygon, threepoverhang).
shape(polygon, trapezium).
shape(polygon, triangle).
shape(polygon, tripleoctagon).
shape(polygon, utr).

%! style(?Class:oneof([edge,node]), ?Name:atom) is nondet.

style(edge, bold).
style(edge, dashed).
style(edge, dotted).
style(edge, solid).

style(node, bold).
style(node, dashed).
style(node, diagonals).
style(node, dotted).
style(node, filled).
style(node, rounded).
style(node, solid).
style(node, striped).
style(node, wedged).

%! gv_typecheck(+Type:compound, +Value:term) is semidet.
% Succeeds of the given value is of the given GraphViz type.
%
% @arg Type A compound term representing a GraphViz type.
% @arg Value The atomic name of a value.
%
% @tbd Test the DCG for HTML labels.
% @tbd Add the escape sequences for the =escString= datatype.

gv_typecheck(or(AlternativeTypes), Value):-
  member(Type, AlternativeTypes),
  gv_typecheck(Type, Value),
  !.
gv_typecheck(escString, Value):-
  typecheck(atom, Value),
  !.
gv_typecheck(lblString, Value):-
  gv_typecheck(escString, Value),
  !.
gv_typecheck(lblString, Value):-
  gv_typecheck(htmlLabel, Value),
  !.
gv_typecheck(polygon_based_shape, Value):-
  findall(Shape, shape(polygon, Shape), Shapes),
  typecheck(oneof(Shapes), Value).
gv_typecheck(Type, Value):-
  typecheck(Type, Value).



% STREAMING %

%! stream_attribute(+Stream:stream, +Attribute:nvpair, +Separator:atom) is det.
% Writes an attribute, together with a separator.
%
% @arg Stream An output stream.
% @arg Attribute A name-value pair.
% @arg Separator An atomic separator.

stream_attribute(Stream, Separator, Attribute):-
  Attribute =.. [Name, Value],
  (
    attribute(Name, _Type, _Context, _Attributes, _Default)
  ->
    c_convert(Value, C_Value),
    format(Stream, '~w="~w"~w', [Name, C_Value, Separator])
  ;
    true
  ).

%! stream_attributes(
%!   +Stream:stream,
%!   +Attributes:list(nvpair),
%!   +Separator:atom
%! ) is det.
% Writes the given list of attributes to an atom.
%
% @arg Stream An output stream.
% @arg Attributes A list of name-value pairs.
% @arg Separator An atomic separator that is written between the attributes.

% Empty attribute list.
stream_attributes(_Stream, [], _Separator):-
  !.
% Non-empty attribute list.
% Write the open and colsing signs of the attribute list.
stream_attributes(Stream, Attributes, Separator):-
  format(Stream, '[', []),
  stream_attributes0(Stream, Attributes, Separator),
  format(Stream, ']', []).

% We know that the list in not empty.
% For the last attribute in the list we use the empty separator.
stream_attributes0(Stream, [Attribute], _Separator):-
  !,
  stream_attribute(Stream, '', Attribute).
stream_attributes0(Stream, [Attribute | Attributes], Separator):-
  stream_attribute(Stream, Separator, Attribute),
  stream_attributes0(Stream, Attributes, Separator).

%! stream_edge(+Stream:stream, +Edge:edge) is det.
% Writes an edge term.
%
% @arg Stream An output stream.
% @arg Edge A GraphViz edge compound term.

stream_edge(Stream, edge(FromVertexID, ToVertexID, EdgeAttributes)):-
  print_indent(Stream, 1),
  format(Stream, 'node_~w -> node_~w ', [FromVertexID, ToVertexID]),
  stream_attributes(Stream, EdgeAttributes, ', '),
  format(Stream, ';', []),
  nl(Stream).

%! stream_graphviz(+Stream:stream, +GraphElement:compound) is det.
% Writes a GraphViz structure to an output stream.
%
% @arg Stream An output stream.
% @arg GraphElement A GraphViz graph compound term of the form
%        =|graph(Vertices, Edges, GraphAttributes)|=.

stream_graphviz(Stream, graph(Vertices, Edges, GraphAttributes)):-
  option(label(GraphName), GraphAttributes, noname),
  format(Stream, 'digraph ~w {', [GraphName]),
  nl(Stream),
  maplist(vertex_to_dom(Stream), Vertices),
  nl(Stream),
  maplist(stream_edge(Stream), Edges),
  nl(Stream),
  stream_graph_attributes(Stream, GraphAttributes),
  format(Stream, '}', []),
  nl(Stream),
  flush_output(Stream).

%! stream_graph_attributes(
%!   +Stream:stream,
%!   +GraphAttributes:list(nvpair)
%! ) is det.
% Writes the given GraphViz graph attributes.
%
% The writing of graph attributes deviates a little bit from the writing of
% edge and node attributes, because the written attributes are not enclosed in
% square brackets and they are written on separate lines (and not as
% comma-separated lists).
%
% @arg Stream An output stream.
% @arg GraphAttributes A list of name-value pairs.

stream_graph_attributes(Stream, GraphAttributes):-
  print_indent(Stream, 1),
  stream_attributes0(Stream, GraphAttributes, '\n  '),
  nl(Stream).

%! vertex_to_dom(+Stream:stream, +Vertex:vertex) is det.
% Writes a vertex term.
%
% @arg Stream An output stream.
% @arg Vertex A GraphViz vertex compound term.

vertex_to_dom(Stream, node(VertexID, VerticeAttributes)):-
  print_indent(Stream, 1),
  format(Stream, 'node_~w ', [VertexID]),
  stream_attributes(Stream, VerticeAttributes, ', '),
  format(Stream, ';', []),
  nl(Stream).

