:- module(
  jgf,
  [
    jgf/3,      % +Graph, :NodeLabel_2, -Dict
    jgf_reply/2 % +Graph, :NodeLabel_2
  ]
).

/** <module> JSON Graph Format (JGF)

@author Wouter Beek
@version 2017/11
*/

:- use_module(library(apply)).
:- use_module(library(http/json)).

:- meta_predicate
    jgf(+, 2, -),
    jgf_edge(2, +, -),
    jgf_node(2, +, -),
    jgf_reply(+, 2).





%! jgf(+Graph:compound, :NodeLabel_2, -Dict:dict) is det.

jgf(
  graph(Nodes1,Edges1),
  NodeLabel_2,
  _{graph: _{directed: true, edges: Edges2, nodes: Nodes2}}
):-
  maplist(jgf_node(NodeLabel_2), Nodes1, Nodes2),
  maplist(jgf_edge(NodeLabel_2), Edges1, Edges2).



%! jgf_edge(:NodeLabel_2, +Edge:compound, -Dict:dict) is det.

jgf_edge(NodeLabel_2, edge(S,P,O), _{label: Label, source: S, target: O}) :-
  call(NodeLabel_2, P, Label).



%! jgf_node(:NodeLabel_2, +Node, -Dict:dict) is det.

jgf_node(NodeLabel_2, Node, _{id: Node, label: Label}) :-
  call(NodeLabel_2, Node, Label).



%! jgf_reply(+Graph:compound, :NodeLabel_2) is det.

jgf_reply(Graph, NodeLabel_2) :-
  jgf(Graph, NodeLabel_2, Dict),
  format("Content-Type: application/vnd.jgf+json\n\n"),
  json_write_dict(current_output, Dict).
