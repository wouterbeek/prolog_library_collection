:- module(
  rdf_web,
  [
    rdf_graphs_web/1, % -Markup:dom
    %rdf_load_web/2, % +Graph:atom
    %                % -Markup:dom
    %rdf_load_web/3, % +Graph:atom
    %                % +Format:oneof([turtle,xml])
    %                % -Markup:dom
    rdf_namespaces_web/1, % -Markup:dom
    rdf_namespaces_web/2 % +Graph:atom
                         % -Markup:dom
    %rdf_save_web/2 % +Graph:atom
    %               % -Markup:dom
  ]
).

/** <module> RDF Web

Web predicates for RDF graphs.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03-2013/05
*/

:- use_module(html(html)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_namespace)).
:- use_module(rdf(rdf_serial)).
:- use_module(xml(xml_namespace)).



%% rdf_graphs_web(-Markup:list) is det.
% Returns the markup for an enumeration of the currently loaded graphs.
%
% @param Markup A list of HTML markup elements.

rdf_graphs_web(Markup):-
  findall(
    [Graph, Triples],
    rdf_statistics(triples_by_graph(Graph, Triples)),
    List
  ),
  (
    List == []
  ->
    Markup = [element(p, [], ['There are no loaded RDF graphs.'])]
  ;
    list_to_table(
      [header(true)],
      [['Graph', 'Number of triples'] | List],
      Table
    ),
    Markup = [element(p, [], ['The currently loaded graphs:']), Table]
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
  rdf_load2(File, Graph),
  !,
  Markup =
    [element(p, [], ['Graph ', Graph, ' was loaded from file ', File, '.'])].
rdf_load_web(Graph, Markup):-
  Markup = [element(p, [], ['An RDF graph named ', Graph,
    ' could not be found in the personal data directory.'])].

%% rdf_namespaces_web(-Markup:list) is det.
% Returns a list of the currently defined namespaces in HTML markup format.
%
% @param Markup A list of HTML markup elements.

rdf_namespaces_web(Markup):-
  xml_current_namespaces(Namespaces),
  rdf_namespaces_web0(Namespaces, Table),
  Markup = [element(p, [], ['The currently loaded namespaces:']), Table].

%% rdf_namespaces_web(+Graph:atom, -Markup:list) is det.
% Returns a list of the namespaces that occur in a specific graph,
% in HTML markup format.
%
% @param Graph The atomic name of a graph.
% @param Markup A list of HTML markup elements.

rdf_namespaces_web(Graph, Markup):-
  rdf_current_namespaces(Graph, Namespaces),
  rdf_namespaces_web0(Namespaces, Table),
  Markup = [element(p, [], ['The namespaces in graph ', Graph, '.']), Table].

%% rdf_namespaces_web0(+Namespaces:list(atom), -Table:dom) is det.
% Returns the markup for an enumeration of the given namespaces.
%
% @param Namespaces A list of atomic names of namespaces.
% @param Markup A list of HTML markup elements.

rdf_namespaces_web0(Namespaces, Table):-
  findall(
    [Prefix, URI],
    (
      member(Prefix, Namespaces),
      rdf_current_prefix(Prefix, URI)
    ),
    List
  ),
  list_to_table([header(true)], [['Prefix', 'URI'] | List], Table).

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

