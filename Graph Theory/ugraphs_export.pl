:- module(
  ugraphs_export,
  [
    ugraph_edge_coloring/3, % +Options:list(nvpair)
                            % +Edge:edge
                            % -Color:color
    ugraph_edge_naming/3, % +Options:list(nvpair)
                          % +Edge:edge
                          % -Name:atom
    ugraph_edge_styling/3, % +Options:list(nvpair)
                           % +Edge:edge
                           % -Style:pair(oneof([bold,dashed,dotted,solid]),atom)
    ugraph_graph_naming/2 % +Options:list(nvpair)
                          % -Name:atom
  ]
).

/** <module> Ugraphs export

Predicates that are exclusively used to export ugraphs.

@author Wouter Beek
@version 2013/02-2013/03
*/

:- use_module(graph_theory(graph_generic)).



ugraph_edge_coloring(_Options, _Edge, black).

%! ugraph_edge_naming(+Options:list(nvpair), +Edge:edge, -Name:atom) is det.
% Returns a name for the given edge.
%
% @arg Options A list of name-value pairs.
% @arg Edge An edge of the form =|FromVertex-ToVertex|=.
% @arg Name An atomic name for the given edge.

ugraph_edge_naming(Options, FromVertex-ToVertex, Name):-
  maplist(
    vertex_naming(Options),
    [FromVertex, ToVertex],
    [FromVertexName, ToVertexName]
  ),
  format(atom(Name), '~w->~w', [FromVertexName, ToVertexName]).

ugraph_edge_styling(_Options, _FromVertex-_ToVertex, solid/normal).

ugraph_graph_naming(Options, Name):-
  option(graph(Graph), Options),
  term_to_atom(Graph, Name).

