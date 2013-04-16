:- module(
  rdf_web,
  [
    namespaces_web/1, % -Markup:dom
    namespaces_web/2, % +Graph:atom
                      % -Markup:dom
    rdf_graphs_web/1 % -Markup:dom
    %rdf_load_web/2, % +Graph:atom
    %                % -Markup:dom
    %rdf_load_web/3, % +Graph:atom
    %                % +Format:oneof([turtle,xml])
    %                % -Markup:dom
    %rdf_save_web/2 % +Graph:atom
    %               % -Markup:dom
  ]
).

/** <module> RDF Web

Web predicates for RDF graphs.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03
*/

:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_serial)).



format_file_type(xml, rdf).
format_file_type(turtle, turtle).

%% namespaces_web(-Markup:list) is det.
% Returns a list of the currently defined namespaces in HTML markup format.
%
% @param Markup A list of HTML markup elements.

namespaces_web([element(p, [], ['The currently loaded namespaces:']), List]):-
  rdf_current_namespaces(Namespaces),
  namespaces_web0(Namespaces, List).

%% namespaces_web(+Graph:atom, -Markup:list) is det.
% Returns a list of the namespaces that occur in a specific graph,
% in HTML markup format.
%
% @param Graph The atomic name of a graph.
% @param Markup A list of HTML markup elements.

namespaces_web(
  Graph,
  [element(p, [], ['The namespaces in graph ', Graph]), List]
):-
  rdf_current_namespaces(Graph, Namespaces),
  namespaces_web0(Namespaces, List).

%% namespaces_web0(+Namespaces:list(atom), -Markup:list) is det.
% Returns the markup for an enumeration of the given namespaces.
%
% @param Namespaces A list of atomic names of namespaces.
% @param Markup A list of HTML markup elements.

namespaces_web0(Namespaces, element(ol, [], ListItems)):-
  findall(
    element(li, [], [Namespace]),
    member(Namespace, Namespaces),
    ListItems
  ).

%% rdf_graphs_web(-Markup:list) is det.
% Returns the markup for an enumeration of the currently loaded graphs.
%
% @param Markup A list of HTML markup elements.

rdf_graphs_web(Markup):-
  findall(
    element(li, [], [Graph]),
    rdf_graph(Graph),
    GraphsMarkup
  ),
  (
    GraphsMarkup == []
  ->
    Markup = [element(p, [], ['There are no loaded RDF graphs.'])]
  ;
    Markup = [
      element(p, [], ['The currently loaded graphs:']),
      element(ol, [], GraphsMarkup)
    ]
  ).

%% rdf_load_web(+Graph:atom, -Markup:list) is det.
% Loads the graph with the given name into memory.
% Graphs that are loaded via this front-end should be located in the user's
% =data= subdirectory.
%
% @param Graph The atomic name of a graph.
% @param Markup

% Prefer turtle.
rdf_load_web(Graph, Markup):-
  rdf_load_web(Graph, _SerializationFormat, Markup),
  !.
rdf_load_web(Graph, Markup):-
  Markup = [element(p, [], ['An RDF graph named ', Graph,
      ' could not be found in the personal data directory.'])].

rdf_load_web(Graph, SerializationFormat, Markup):-
  rdf_load2(File, SerializationFormat, Graph),
  !,
  Markup =
      [element(p, [], [
        'Graph ',
        Graph,
        ' was loaded from file ',
        File,
        ' in serialization ',
        SerializationFormat,
        '.'
      ])].
rdf_load_web(Graph, _SerializationFormat, Markup):-
  Markup = [element(p, [], [
      'No graph with name ',
      Graph,
      ' could not be loaded the personal data directory.'
  ])].

%% rdf_save_web(+Graph:atom, -Markup:dom) is det.
% Saves the RDF graph with the given name from the Web interface.

rdf_save_web(Graph, Markup):-
  rdf_save2(Graph, SerializationFormat, File),
  !,
  Markup =
      [element(p, [], [
        'Graph ',
        Graph,
        ' was saved to file ',
        File,
        'using serialization format ',
        SerializationFormat,
        '.'
      ])].
rdf_save_web(Graph, Markup):-
  Markup = [element(p, [], ['Graph', Graph, ' could not be saved.'])].

