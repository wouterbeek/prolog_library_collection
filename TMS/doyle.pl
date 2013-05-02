:- module(
  doyle,
  [
    add_justification/4, % +Ins:list(node)
                         % +Outs:list(node)
                         % +Consequence:node
                         % -Justification:justification
    add_node/1, % -Node:node
    affected_consequence/2, % +Node:node
                            % -AffectedConsequence:node
    affected_consequences/2, % +Node:node
                             % -AffectedConsequences:ord_set(node)
    antecedent/2, % +Node:node
                  % -Antecedent:node
    antecedents/2, % +Node:node
                   % -Antecedents:ord_set(node)
    assumption/1, % ?Node:node
    believe/1, % ?Node:node
    believed_consequence/2, % ?Node:node
                            % ?BelievedConsequence:node
    believed_consequences/2, % +Node:node
                             % -BelievedConsequences:ord_set(node)
    believed_repercussions/2, % +Node:node
                              % -Repercussions:ord_set(node)
    consequence/2, % ?Node:node
                   % ?Consequence:node
    consequences/2, % +Node:node
                    % -Consequence:ord_set(node)
    export_tms/1, % +File:atom
    foundations/2, % +Node:node
                   % -Foundations:ord_set(node)
    in_node/1, % ?Node:node
    justification/1, % ?Justification:justification
    justification_consequence/2, % ?Justification:justification
                                 % ?Consequence:node
    justification_id/2, % ?Justification:justification
                        % ?ID:number
    justification_in/2, % ?Justification:justification
                        % ?In:node
    justification_out/2, % ?Justification:justification
                         % ?Out:node
    node/1, % ?Node:node
    node_id/2, % ?Node:node
               % ?ID:number
    out_node/1, % ?Node:node
    premise/1, % ?Justification:justification
    repercussions/2, % +Node:node
                     % -Repercussions:ord_set(node)
    reset_tms/0,
    supporting_justifications/2, % +Node:node
                                 % -SupportingJustifications:ord_set(justification)
    supporting_node/2, % ?Node:node
                       % ?SupportingNode:node
    supporting_nodes/2, % +Node:node
                        % -SupportingNodes:ord_set(node)
    support_status/2, % ?Node:node
                      % ?Status:atom
    test/0,
    valid/1 % ?Justification:justification
  ]
).

/** <module> Doyle

The TMS as described in Jon Doyle, 1979, _|A Truth Maintenance System|_.

@author Wouter Beek
@version 2012/06, 2013/05
*/

:- use_module(generics(file_ext)).
:- use_module(generics(meta_ext)).
:- use_module(generics(os_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(standards(graphviz)).

:- rdf_register_ns(doyle, 'http://www.wouterbeek.com/doyle.owl#', [force(true)]).

:- rdf_meta(rdf_assert_integer(r,r,?,+)).
:- rdf_meta(rdf_integer(r,r,?,+)).



%% add_justification(
%%   +Ins:list(node),
%%   +Outs:list(nodes),
%%   +Consequence:node,
%%   -Justification:justification
%% ) is det.
% Adds an SL-justification.
%
% @param Ins
% @param Outs
% @param Consequence
% @param Justification

add_justification(Ins, Outs, Consequence, Justification):-
  % Type checking.
  maplist(node, Ins),
  maplist(node, Outs),
  node(Consequence),

  % Create the new justification.
  flag(justifications, ID, ID + 1),
  format(atom(Name), 'j~w', [ID]),
  rdf_global_id(doyle:Name, Justification),
  rdf_assert(Justification, rdf:type, doyle:justification),
  rdf_assert_datatype(Justification, doyle:has_id, integer, ID, doyle),

  % Assert the properties of the justification.
  forall(
    member(In, Ins),
    rdf_assert(Justification, doyle:has_in, In, doyle)
  ),
  forall(
    member(Out, Outs),
    rdf_assert(Justification, doyle:has_out, Out, doyle)
  ),
  rdf_assert(Justification, doyle:has_consequence, Consequence, doyle).
  %truth_maintenance(Justification, Consequence).

%% add_node(-Node:node) is det.
% Adds a node.
%
% @param Node

add_node(Node):-
  flag(nodes, ID, ID + 1),
  format(atom(Name), 'n~w', [ID]),
  rdf_global_id(doyle:Name, Node),
  rdfs_assert_individual(Node, doyle:node, doyle),
  rdf_assert_datatype(Node, doyle:has_id, integer, ID, doyle).

%% affected_consequence(?Node:node, ?AffectedConsequence:node) is nondet.
% An affected consequences of a node is a consequences of the node
% that contains the node in its supporting nodes.
%
% @param Node
% @param AffectedConsequence

affected_consequence(Node, AffectedConsequence):-
  consequence(Node, AffectedConsequence),
  supporting_node(AffectedConsequence, Node).

%% affected_consequences(
%%   +Node:node,
%%   -AffectedConsequences:ord_set(node)
%% ) is det.
% The affected consequences of a node are the consequences of the node
% that contain the node in their supporting nodes.
%
% @param Node
% @param AffectedConsequences

affected_consequences(Node, AffectedConsequences):-
  setoff(
    AffectedConsequence,
    affected_consequence(Node, AffectedConsequence),
    AffectedConsequences
  ).

%% antecedents(?Node:node, ?Antecedent:node) is nondet.
% An antecedent is a supporting node of a believed node.
%
% @param Node
% @param Antecedent

antecedent(Node, Antecedent):-
  believe(Node),
  supporting_node_in_(Node, Antecedent).

%% antecedents(+Node:node, -Antecedents:ord_set(node)) is det.
% Returns the antecedents of the given node.
%
% @param Node
% @param Antecedents

antecedents(Node, Antecedents):-
  setoff(
    Antecedent,
    antecedent(Node, Antecedent),
    Antecedents
  ).

%% assumption(Node) is nondet.
% A assumption node is a node with a nonempty outlist for its
% supporting justification.
%
% @param Node

assumption(Node):-
  node(Node),
  supporting_justification(Justification, Node),
  justification_out(Justification, _Out).

%% believe(?Node:node) is nondet.
% A believed node.
% A node that is in.
%
% @param Node.

believe(Node):-
  in_node(Node).

%% believed_consequence(+Node:node, -BelievedConsequence:node) is det.
% Returns a believed consequence of the given node.
%
% @param Node
% @param BelievedConsequence

believed_consequence(Node, BelievedConsequence):-
  consequence(Node, BelievedConsequence),
  antecedent(BelievedConsequence, Node).

%% believed_consequences(
%%   +Node:node,
%%   -BelievedConsequences:ord_set(node)
%% ) is det.
% Returns the believed consequences of the given node.
%
% @param Node
% @param BelievedConsequence

believed_consequences(Node, BelievedConsequences):-
  setoff(
    BelievedConsequence,
    believed_consequence(Node, BelievedConsequence),
    BelievedConsequences
  ).

%% believed_repercussions(+Node:node, -Repercussions:ord_set(node)) is det.
% Returns the believed repercussions of the given node.
%
% @param Node
% @param Repercussions

believed_repercussions(Node, Repercussions):-
  transitive_closure(believed_consequences, Node, Repercussions).

%% consequence(+Node:node, -Consequence:node) is nondet.
% A consequence of a node is a node which mentions the prior node
% in one of its justifications.
%
% @param Node
% @param Consequence

consequence(Node, Consequence):-
  (
    justification_in(Justification, Node)
  ;
    justification_out(Justification, Node)
  ),
  justification_consequence(Justification, Consequence).

%% consequences(+Node:node, -Consequences:ord_set(node)) is det.
% Returns the consequences of the given node.
%
% @param Node
% @param Consequences

consequences(Node, Consequences):-
  setoff(
    Consequence,
    consequence(Node, Consequence),
    Consequences
  ).

%% export_tms(+File:atom) is det.
% Exports the TMS to GraphViz.

export_tms(GV_File):-
  open(GV_File, write, Stream, []),
  format(Stream, 'digraph circuit {\n', []),
  forall(
    node(Node),
    (
      node_id(Node, NodeID),
      format(
        Stream,
        '  n~w [color="green", fontsize="11", label="~w", shape="ellipse", style="solid"];\n',
        [NodeID, NodeID]
      )
    )
  ),
  forall(
    justification(Justification),
    (
      justification_id(Justification, JustificationID),
      format(
        Stream,
        '  j~w [color="blue", fontsize="11", label="~w", shape="box", style="solid"];\n',
        [JustificationID, JustificationID]
      )
    )
  ),
  format(Stream, '\n', []),
  forall(
    justification_in(Justification, Node),
    (
      node_id(Node, NodeID),
      justification_id(Justification, JustificationID),
      format(
        Stream,
        '  n~w -> j~w [color="black"];\n',
        [NodeID, JustificationID]
      )
    )
  ),
  forall(
    justification_out(Justification, Node),
    (
      node_id(Node, NodeID),
      justification_id(Justification, JustificationID),
      format(
        Stream,
        '  n~w -> j~w [color="red"];\n',
        [NodeID, JustificationID]
      )
    )
  ),
  forall(
    justification_consequence(Justification, Node),
    (
      node_id(Node, NodeID),
      justification_id(Justification, JustificationID),
      format(
        Stream,
        '  j~w -> n~w [color="black"];\n',
        [JustificationID, NodeID]
      )
    )
  ),
  format(Stream, '\n', []),
  format(Stream, '  charset="UTF-8"\n', []),
  format(Stream, '  fontsize="11"\n', []),
  format(Stream, '  label="Doyle"\n', []),
  format(Stream, '  overlap=false\n', []),
  format(Stream, '}\n', []),
  close(Stream),
  file_type_alternative(GV_File, svg, SVG_File),
  convert_graphviz(GV_File, dot, svg, SVG_File).

%% foundations(+Node:node, -Foundations:ord_set(node)) is det.
% Returns the foundations of the given node.
% The foundations are the transitive closure of the antecedents.
%
% @param Node
% @param Foundations

foundations(Node, Foundations):-
  transitive_closure(antecedents, Node, Foundations).

%% in_node(?Node:node) is nondet.
% A node that is in.
%
% @param Node

in_node(Node):-
  justification_consequence(Justification, Node),
  valid(Justification).

%% justification(Justification) is nondet.
% A justification.
%
% @param Justification

justification(Justification):-
  rdfs_individual_of(Justification, doyle:justification).

%% justification_consequence(Justification, Node) is nondet.
% A justification and its consequence.
%
% @param Justification
% @param Node

justification_consequence(Justification, Node):-
  var(Justification),
  var(Node),
  !,
  justification(Justification),
  justification_consequence_(Justification, Node).
justification_consequence(Justification, Node):-
  justification_consequence_(Justification, Node),
  !.

justification_consequence_(Justification, Node):-
  rdf(Justification, doyle:has_consequence, Node, doyle).

%% justification_id(Justification, ID) is nondet.
% A justification and its identifier.
%
% @param Justification
% @param ID

justification_id(Justification, ID):-
  var(Justification),
  var(ID),
  !,
  justification_id_(Justification, ID).
justification_id(Justification, ID):-
  justification_id_(Justification, ID),
  !.

justification_id_(Justification, ID):-
  rdf_datatype(Justification, doyle:has_id, integer, ID, doyle).

%% justification_in(Justification, Node) is nondet.
% An SL-justification and one of its innodes.
%
% @param Justification An SL-justification.
% @param Node

justification_in(Justification, Node):-
  rdf(Justification, doyle:has_in, Node, doyle).

%% justification_out(Justification, Node) is nondet.
% An SL-justification and one of its outnodes.
%
% @param Justification An SL-justification.
% @param Node

justification_out(Justification, Node):-
  rdf(Justification, doyle:has_out, Node, doyle).

%% node(Node) is nondet.
% A node.

node(Node):-
  rdfs_individual_of(Node, doyle:node).

%% node_id(Node, ID) is nondet.
% A node and its identifier.
%
% @param Node
% @param ID

node_id(Node, ID):-
  var(Node),
  var(Node),
  !,
  node(Node),
  node_id_(Node, ID).
node_id(Node, ID):-
  node_id_(Node, ID),
  !.

node_id_(Node, ID):-
  rdf_datatype(Node, doyle:has_id, integer, ID, doyle).

%% out_node(?Node:node) is nondet.
% A node that is out.
%
% @param Node

out_node(Node):-
  \+(in_node(Node)).

%% premise(?Justification:justification) is nondet.
% A premise justification.
%
% @param Justification

premise(Justification):-
  justification(Justification),
  findall(
    In,
    justification_in(Justification, In),
    []
  ),
  findall(
    Out,
    justification_out(Justification, Out),
    []
  ).

repercussions(Node, Repercussions):-
  transitive_closure(affected_consequences, Node, Repercussions).

%% reset_tms is det.
% Resets the TMS.

reset_tms:-
  flag(justifications, _, 1),
  flag(nodes, _, 1).

%% statistics is det.
% Opens a web page with statistics of the TMS.

/*
write_doyle(_Request):-
  findall(
    tr([
      td(Node),
      td(SupportStatus),
      td(SupportingJustifications),
      td(SupportingNodes),
      td(Antecedents),
      td(Foundations),
      %td(Ancestors),
      td(Consequences),
      td(AffectedConsequences),
      td(BelievedConsequences),
      td(Repercussions),
      td(BelievedRepercussions)
    ]),
    (
      node(Node),
      support_status(Node, SupportStatus),
      supporting_justifications(Node, SupportingJustifications),
      supporting_nodes(Node, SupportingNodes),
      antecedents(Node, Antecedents),
      foundations(Node, Foundations),
      %ancestors(Node, Ancestors),
      consequences(Node, Consequences),
      affected_consequences(Node, AffectedConsequences),
      believed_consequences(Node, BelievedConsequences),
      repercussions(Node, Repercussions),
      believed_repercussions(Node, BelievedRepercussions)
    ),
    Rows
  ),
  reply_html_page(
    title('Doyle'),
    [
      a(href('index.html'), 'Index'),
      table(
        [align(center), border(1), width('80%')],
        [tr([
          th('Node'),
          th('SupportStatus'),
          th('SupportingJustifications'),
          th('SupportingNodes'),
          th('Antecedents'),
          th('Foundations'),
          %th('Ancestors'),
          th('Consequences'),
          th('AffectedConsequences'),
          th('BelievedConsequences'),
          th('Repercussions'),
          th('BelievedRepercussions')
          | Rows
        ])]
      )
    ]
  ).
*/

supporting_justifications(Node, SupportingJustifications):-
  setoff(
    SupportingJustification,
    justification_consequence(SupportingJustification, Node),
    SupportingJustifications
  ).

supporting_node(Node, SupportingNode):-
  in_node(Node),
  !,
  supporting_node_in_(Node, SupportingNode).
supporting_node(Node, SupportingNode):-
  out_node(Node),
  supporting_node_out_(Node, SupportingNode).

supporting_node_in_(Node, SupportingNode):-
  supporting_justification(Node, Justification),
  (
    justification_in(Justification, SupportingNode)
  ;
    justification_out(Justification, SupportingNode)
  ).

supporting_node_out_(Node, SupportingNode):-
  supporting_nodes_out(Node, SupportingNodes),
  member(SupportingNode, SupportingNodes).

%% supporting_nodes(+Node:node, -SupportingNodes:ord_set(node)) is det.
% Supporting nodes of a node are the set of nodes the TMS used
% to determine the prior node's support-status.
%   1. For an in node the supporting nodes are the inlist and outlist
%      nodes.
%   2. For an out node a supporting node is picked from each
%      justification: either an out node from the inlist or
%      an in node from the outlist.
%
% @param Node
% @param SupportingNodes

supporting_nodes(Node, SupportingNodes):-
  in_node(Node),
  !,
  supporting_nodes_in(Node, SupportingNodes).
supporting_nodes(Node, SupportingNodes):-
  out_node(Node),
  supporting_nodes_out(Node, SupportingNodes).

supporting_nodes_in(Node, SupportingNodes):-
  setoff(
    SupportingNode,
    supporting_node(Node, SupportingNode),
    SupportingNodes
  ).

supporting_nodes_out(Node, SupportingNodes):-
  setoff(
    Justification,
    justification_consequence(Justification, Node),
    Justifications
  ),
  supporting_nodes_out(Justifications, SupportingNodes).

supporting_nodes_out([], []).
supporting_nodes_out([Justification | Justifications], [Node | Nodes]):-
  justification_in(Justification, Node),
  supporting_nodes_out(Justifications, Nodes).
supporting_nodes_out([Justification | Justifications], [Node | Nodes]):-
  justification_out(Justification, Node),
  supporting_nodes_out(Justifications, Nodes).

%% support_status(?Node:node, ?SupportStatus:atom) is nondet.
% A node and its support status.
%
% @param Node
% @param SupportStatus Either 'in', 'out', or 'nil'.

support_status(Node, in):-
  in_node(Node),
  !.
support_status(Node, out):-
  out_node(Node),
  !.
support_status(_Node, nil).

%% test/0
% Tests the TMS for the example that Doyle gave.

test:-
  reset_tms,
  add_node(A),
  add_node(B),
  add_node(C),
  add_node(D),
  add_node(E),
  add_node(F),
  add_justification([C], [], A, _),
  add_justification([], [A], B, _),
  add_justification([A], [], C, _),
  add_justification([B], [], D, _),
  add_justification([C], [], D, _),
  add_justification([], [], E, _),
  add_justification([C, E], [], F, _),
  absolute_file_name(data(export), File, [access(write), file_type(dot)]),
  export_tms(File).

/*
truth_maintenance(Justification, Node):-
  in_node(Node),
  !.
truth_maintenance(Justification, Node):-
  out_node(Node),
  \+(valid(Justification)),
  !.
truth_maintenance(Justification, Node):-
  affected_consequences(Node, AffectedConsequences),
  truth_maintenance2(Justification, Node, AffectedConsequences).

truth_maintenance2(_Justification, _Node, []):-
  !.
truth_maintenance2(_Justification, Node, AffectedConsequences):-
  repercussions(Node, Repercussions),
  ord_add_element(Repercussions, Node, Repercussions_),
*/

%% valid(Justification) is nondet.
% A valid justification.
%
% @param Justification

valid(Justification):-
  justification(Justification),
  forall(
    justification_in(Justification, In),
    believe(In)
  ),
  forall(
    justification_out(Justification, Out),
    \+(believe(Out))
  ).

