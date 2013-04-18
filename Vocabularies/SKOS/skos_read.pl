:- module(
  skos_read,
  [
    skos_broader/3, % ?Broader:uri
                    % ?Narrower:uri
                    % ?Graph:atom
    skos_export_hierarchy/1, % +Tree:tree
    skos_narrower/3, % ?Narrower:uri
                     % ?Broader:uri
                     % ?Graph:atom
    tree_to_nodes/2, % +Tree:tree
                     % -Nodes:ord_set(uri)
    tree_to_triples/2 % +Tree:tree
                      % -Triples:ord_set(triple)
  ]
).

/** <module> SKOS READ

@author Wouter Beek
@version 2013/04
*/

:- use_module(graph_theory(graph_export)).
:- use_module(graph_theory(graph_generic)).
:- use_module(generics(meta_ext)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_namespace)).

:- rdf_register_namespace(skos).

:- rdf_meta(skos_export_hierarchy(r)).



skos_broader(Broader, Narrower, Graph):-
  rdf(Broader, skos:broader, Narrower, Graph).

skos_export_hierarchy(Root):-
  rdf_global_id(skos:broader, Predicate),
  beam([], Root, [Predicate], Vertices, Edges),
  write_graph(
    [edges(Edges), out(graphviz), vertices(Vertices)],
    'STCN_Topics',
    File
  ),
  write(File).

skos_narrower(Narrower, Broader, Graph):-
  rdf(Narrower, skos:narrower, Broader, Graph).

tree_to_triples(Nodes, AllTriples):-
  setoff(
    rdf(Node, skos:broader, DownNode),
    (
      member(Node, Nodes),
      rdf(Node, skos:broader, DownNode)
    ),
    Triples
  ),
  setoff(
    DownNode,
    member(rdf(_Node, _, DownNode), Triples),
    DownNodes
  ),
  tree_to_triples(DownNodes, DownTriples),
  ord_union(Triples, DownTriples, AllTriples).

tree_to_nodes(Nodes, AllNodes):-
  setoff(
    DownNode,
    (
      member(Node, Nodes),
      skos_broader(Node, DownNode, _Graph)
    ),
    DownNodes
  ),
  tree_to_nodes(DownNodes, AllDownNodes),
  ord_union(Nodes, AllDownNodes, AllNodes).

