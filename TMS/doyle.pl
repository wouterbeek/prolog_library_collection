:- module(
  doyle,
  [
    add_justification/5, % +Ins:list(node)
                         % +Outs:list(node)
                         % +Message:atom
                         % +Consequence:node
                         % -Justification:justification
    add_node/2, % +Statement:atom
                % -Node:node
    reset_tms/0,
    test/0
  ]
).

/** <module> Doyle

The TMS as described in Jon Doyle, 1979, _|A Truth Maintenance System|_.

---+ Idea

Rational thought is the process of finding reasons for attitudes.

The only _real_ component of thought is the current set of reasons -
the attitudes such as beliefs and desires arise from the set of seasons,
and have no independent existence.

To study rational thought, we whould study justified belief or reasoned
argument, and ignore questions of truth. Truth enters into the study of
extra-psychological rationality.

---+ Definitions

---++ Affected consequences

For a node, those consequences of the node which contain the node in
their set of supporting nodes.

---++ Ancestors

For a node, the transitive closure of its *|supporting nodes|*.

The ancestors are related to the nodes that may affect the support status
of the node in any way.

---++ Antecedents

For an _in_ node, its *|supporting nodes|*.

For an _out_ node, nothing.

---++ Assumption

A *justification* with non-empty _out_-list.

A *node* with a *|non-monotonic justification|* as *|well-founded support|*,
i.e. the nodes that explain support status _in_.

---++ Believed consequences

Of a node, its _in_ consequences that have it in their *antecedents*.

---++ Believed repercussions

For a node, the transitive closure of its believed consequences.

---++ Conditional-proof (CP) justification

A justification that _subtracts_ the dependencies of some nodes
(the *hypotheses* of the hypothetical argument) from the dependencies
of others (the *conclusion* of the hypothetical argument).

---++ Consequences

Of a node, the nodes for which it occurs in one of their justifications.

---++ Foundations

For a node, the transitive closure of its *antecedents*.

The foundations of a node are the nodes involved in the
*|well-founded argument|* for belief in the node.

The foundation is realted to the notion of *|well-foundedness|*.

---++ In

A belief _b_ is _in_ iff a justification _|<In,Out>|_ of _b_ has
_|in(b')|_ for all _b'_ in _In_ and _|out(b')|_ for all _b'_ in _Out_.

A node with a *valid* justification is _in_.

---++ Justification

A representation of a *justification*.

There are two types of justification:
    1. Support-list (SL)
    2. Conditional-proof (CP)

The *|external form|* of a justification is only significant for the problem
solver.

The *|internal form|* of a justification is only significant for the TMS.

---++ Justification-set

For a node, the set of its *justifications*.

---++ Node

A representation of a *belief*.

A node is believed iff one of its justifications
(in its *|justification-set|*) is *valid*.

---++ Reason

For a belief _b_, a pair _|<In,Out>|_ of sets of beliefs.

A *|valid reason|* is one whose nodes in the first set are all believed
and whose nodes in the second set are all disbelieved.

---++ Repercussions

Of a node, the transitive closure of its *|affected-consequences|*.

---++ Support-list (SL) justification

A justification that _sums_ the dependencies of the referenced nodes.

An SL-justification is *valid* iff all nodes in _in_-list are _in_
and all node in _out_-list are _out_.

Special types of SL-justifications:
    1. *Premise*: empty _in_- and _out_-list. Always *valid*.
    2. *Normal deduction*: non-empty _in_-list and empty _out_-list.
    3. *Assumption*: non-empty out-list.

---++ Supporting-nodes

Of a node, the nodes the TMS uses to determine its *|support status|*.

For an _in_ node, the nodes in the _in_- and _out_-lists of its
*|well-supported justification|*.

For an _out_-node, the TMS picks one node from each of its justitifications:
    * For *|SL-justitifications|*, the TMS picks either an _in_-node from the
      _out_-list, or an _out_-node from the _in_-list.
    * For *|CP-justitications|*, the TMS picks either an _in_-node from the
      _out_hypotheses or an _out_-node from the _in_hypotheses.

---++ Support status

Of a node, whether it is _in_ or _out_.

---++ Well-founded justification

For every node representing a current belief, a justification that is
somehow lifted out.

Well-founded justifications form a non-circular argument for their node.

Only SL-justifications can be well-founded justifications.

@author Wouter Beek
@version 2012/06, 2013/05
*/

:- use_module(generics(meta_ext)).
:- use_module(generics(os_ext)).
:- use_module(generics(set_theory)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_build)).
:- use_module(rdf(rdf_read)).
:- use_module(rdfs(rdfs_build)).
:- use_module(tms(tms_export)).

:- rdf_register_ns(doyle, 'http://www.wouterbeek.com/doyle.owl#').

:- rdf_meta(add_justification(+,+,+,r,r)).
:- rdf_meta(add_node(+,r)).



% DOYLE'S ALGORITHM %

%% add_justification(
%%   +Ins:list(node),
%%   +Outs:list(node),
%%   +Message:atom,
%%   +Consequence:node,
%%   -Justification:justification
%% ) is det.
% Adds a justification to the TMS.

% Step 1
add_justification(Ins, Outs, Message, Consequence, Justification):-
  maplist(is_node, Ins),
  maplist(is_node, Outs),
  atom(Message),
  is_node(Consequence),
  var(Justification),
  
  % Create the justification.
  flag(justifications, ID, ID + 1),
  format(atom(Name), 'j~w', [ID]),
  rdf_global_id(doyle:Name, Justification),
  rdfs_assert_individual(Justification, doyle:'SL-Justification', doyle),
  rdf_assert_datatype(Justification, doyle:has_id, integer, ID, doyle),
  
  % Update the consequence node.
  add_justification(Consequence, Justification),
  
  % Assert the properties of the justification.
  forall(
    member(In, Ins),
    (
      add_consequence(In, Consequence),
      rdf_assert(Justification, doyle:has_in, In, doyle)
    )
  ),
  forall(
    member(Out, Outs),
    (
      add_consequence(Out, Consequence),
      rdf_assert(Justification, doyle:has_out, Out, doyle)
    )
  ),
  
  % Register CP justification for later processing.
  if_then(
    is_cp_justification(Justification),
    assert(cp_consequence_list(Justification))
  ),
  
  % Update consequent node.
  (
    is_in_node(Consequence)
  ->
    true
  ;
    is_out_node(Consequence)
  ->
    (
      is_valid(Justification)
    ->
      update_belief(Justification, Consequence)
    ;
      % Add either an inlist node that is out, or an outlist node that is in
      % to the supporting nodes of the consequence node.
      (
        member(In, Ins),
        is_out_node(In),
        add_supporting_node(Consequence, In)
      ;
        member(Out, Outs),
        is_in_node(Out),
        add_supporting_node(Consequence, Out)
      ),
      !
    )
  ).


% Step 2
update_belief(Justification, Node):-
  affected_consequences(Node, AffectedConsequences),
  (
    AffectedConsequences == []
  ->
    set_support_status(Node, in),
    forall(
      rdf(Justification, doyle:has_in, In, doyle),
      rdf_assert(Node, doyle:has_supporting_node, In, doyle)
    ),
    forall(
      rdf(Justification, doyle:has_out, Out, doyle),
      rdf_assert(Node, doyle:has_supporting_node, Out, doyle)
    )
  ;
    repercussions(Node, Repercussions),
    marking_the_nodes([Node | Repercussions])
  ).

% Step 3.
marking_the_nodes(Nodes):-
  forall(
    member(Node, Nodes),
    set_support_status(Node, nil)
  ),
  evaluating_node_justifications(Nodes).

% Step 4a
evaluating_justification_set(Node):-
  is_in_node(Node), !.
evaluating_justification_set(Node):-
  is_out_node(Node), !.
evaluating_justification_set(Node):-
  % Enumerate the supporting justifications of this node,
  % binding the SL-justifications before binding the CP-justifications.
  justification(Node, Justification),
  is_valid(Justification),
  !,
  set_supporting_justification(Node, Justification).
evaluating_justifications(Nodes):-
  maplist(evaluating_justification_set, Nodes).



% SUPPORT PREDICATES %

%% add_consequence(+Node:node, +Consequence:node) is det.
% Adds a consequence node to another node.

add_consequence(Node, Consequence):-
  maplist(nonvar, [Node, Consequence]),
  rdf_assert(Node, doyle:has_consequence, Consequence, doyle).

%% add_justification(+Node:node, +Justification:justification) is det.
% Add a justification to a node's justification set.

add_justification(Node, Justification):-
  maplist(nonvar, [Node, Justification]),
  rdf_assert(Node, doyle:has_justification, Justification, doyle).

%% add_node(+Message:atom, -Node:node) is det.
% Adds a node.

add_node(Message, Node):-
  atom(Message),
  var(Node),
  
  flag(nodes, ID, ID + 1),
  format(atom(Name), 'n~w', [ID]),
  rdf_global_id(doyle:Name, Node),
  rdfs_assert_individual(Node, doyle:node, doyle),
  rdf_assert_datatype(Node, doyle:has_id, integer, ID, doyle),
  rdf_assert_datatype(Node, doyle:has_message, string, Message, doyle).

%% add_supporting_node(+Node:node, +SupportingNode:node) is det.

add_supporting_node(Node, SupportingNode):-
  maplist(nonvar, [Node, SupportingNode]),
  rdf_assert(Node, doyle:has_supporting_node, SupportingNode, doyle).

%% affected_consequences(
%%   +Node:node,
%%   -AffectedConsequences:ordset(node)
%% ) is det.
% For a node, those consequences of the node which contain the node in
% their set of supporting nodes.

affected_consequences(Node, AffectedConsequences):-
  setoff(
    AffectedConsequence,
    (
      rdf(Node, doyle:has_consequence, AffectedConsequence, doyle),
      rdf(AffectedConsequence, doyle:has_supporting_node, Node, doyle)
    ),
    AffectedConsequences
  ).

%% antecedents(+Node:node, -Antecedents:ordset(node)) is det.
% An antecedent is a supporting node of a believed node.

antecedents(Node, Antecedents):-
  is_in_node(Node),
  !,
  supporting_nodes(Node, Antecedents).
antecedents(Node, []):-
  is_out_node(Node).

%% assumption(?Assumption:node) is nondet.
% An assumption node is a node with a non-empty outlist for its
% supporting justification.

assumption(Assumption):-
  rdfs_individual_of(Assumption, doyle:'Node'),
  rdf(Assumption, doyle:supporting_justification, Justification, doyle),
  rdf(Justification, doyle:has_out, _Out, doyle).

%% believed_consequences(
%%   +Node:node,
%%   -BelievedConsequences:ordset(node)
%% ) is det.
% Returns a believed consequence of the given node.

believed_consequences(Node, BelievedConsequences):-
  setoff(
    BelievedConsequence,
    (
      rdf(BelievedConsequence, doyle:has_supporting_node, Node, doyle),
      rdf(Node, doyle:has_consequence, BelievedConsequence, doyle)
    ),
    BelievedConsequences
  ).

%% believed_repercussions(+Node:node, -Repercussions:ordset(node)) is det.
% Returns the believed repercussions of the given node.

believed_repercussions(Node, Repercussions):-
  transitive_closure(believed_consequences, Node, Repercussions).

%% consequences(+Node:node, -Consequences:ordset(node)) is det.
% A consequence of a node is a node which mentions the prior node
% in one of its justifications.

consequences(Node, Consequences):-
  setoff(
    Consequence,
    rdf(Node, doyle:has_consequence, Consequence),
    Consequences
  ).

%% cp_justification(?CP_Justification) is nondet.

cp_justification(CP_Justification):-
  rdfs_individual_of(CP_Justification, doyle:'CP-Justification').

%% cp_justification(?Node:node, ?CP_Justification:justification) is nondet.

cp_justification(Node, CP_Justification):-
  rdf(Node, doyle:has_justitification, CP_Justification, doyle),
  cp_justification(CP_Justification).

%% foundations(+Node:node, -Foundations:ordset(node)) is det.
% Returns the foundations of the given node.
% The foundations are the transitive closure of the antecedents.

foundations(Node, Foundations):-
  transitive_closure(antecedents, Node, Foundations).

%% has_support_status(
%%   +Node:node,
%%   +SupportStatus:oneof([in,nil,out])
%% ) is semidet.

has_support_status(Node, SupportStatus):-
  maplist(nonvar, [Node, SupportStatus]),
  support_status(Node, SupportStatus).

%% is_cp_justification(+X) is semidet.

is_cp_justification(Justification):-
  nonvar(Justification),
  cp_justification(Justification).

%% is_in_node(+Node:node) is semidet.

is_in_node(Node):-
  has_support_status(Node, in).

%% is_justification(+X) is semidet.

is_justification(Justification):-
  nonvar(Justification),
  justification(Justification).

%% is_node(+Node:node) is semidet.

is_node(Node):-
  nonvar(Node),
  node(Node).

%% is_out_node(+Node:node) is semidet.

is_out_node(Node):-
  has_support_status(Node, out).

%% is_valid(+Justification:justification) is semidet.
% A valid justification.

is_valid(Justification):-
  is_justification(Justification),
  forall(
    rdf(Justification, doyle:has_in, In, doyle),
    is_in_node(In)
  ),
  forall(
    rdf(Justification, doyle:has_out, Out, doyle),
    is_out_node(Out)
  ).

%% justification(?Justification:justification) is nondet.

justification(CP_Justification):-
  cp_justification(CP_Justification).
justification(SL_Justification):-
  sl_justification(SL_Justification).

%% justification(?Node:node, ?Justification:justification) is nondet.

justification(Node, Justification):-
  sl_justification(Node, Justification).
justification(Node, Justification):-
  cp_justification(Node, Justification).

%% node(?Node:node) is nondet.

node(Node):-
  rdfs_individual_of(Node, doyle:'Node').

%% premise(?Justification:justification) is nondet.

premise(Justification):-
  justification(Justification),
  \+ rdf(Justification, doyle:has_in, _In, doyle),
  \+ rdf(Justification, doyle:has_out, _Out, doyle).

%% repercussions(+Node:node, -Repercussions:ordset(node)) is det.

repercussions(Node, Repercussions):-
  transitive_closure(affected_consequences, Node, Repercussions).

%% reset_tms is det.

reset_tms:-
  flag(justifications, _, 1),
  flag(nodes, _, 1),
  rdf_retractall(_, _, _, doyle).

%% set_support_status(+Node:node, +SupportStatus:oneof([in,nil,out])) is det.

set_support_status(Node, SupportStatus):-
  is_node(Node),
  memberchk(SupportStatus, [in,nil,out]),
  rdf_retractall_datatype(
    Node,
    doyle:has_support_status,
    string,
    _SupportStatus,
    doyle
  ),
  rdf_assert_datatype(
    Node,
    doyle:has_support_status,
    string,
    SupportStatus,
    doyle
  ).

%% set_supporting_justification(
%%   +Node:node,
%%   +SupportingJustification:justification
%% ) is det.

set_supporting_justification(Node, SupportingJustification):-
  maplist(nonvar, [Node, SupportingJustification]),
  rdf_retractall(
    Node,
    doyle:supporting_justification,
    _Justification,
    doyle
  ),
  rdf_assert(
    Node,
    doyle:supporting_justification,
    SupportingJustification,
    doyle
  ).

%% sl_justification(?SL_Justification:justification) is nondet.

sl_justification(SL_Justification):-
  rdfs_individual_of(SL_Justification, doyle:'SL-Justification').

%% sl_justification(?Node:node, ?SL_Justification:justification) is nondet.

sl_justification(Node, SL_Justification):-
  rdf(Node, doyle:has_justification, SL_Justification, doyle),
  rdfs_individual_of(SL_Justification, doyle:'SL-Justification').

%% support_status(?Node:node, ?SupportStatus:oneof([in,out])) is nondet.

support_status(Node, SupportStatus):-
  nonvar(Node),
  !,
  once(support_status0(Node, SupportStatus)).
support_status(Node, SupportStatus):-
  support_status0(Node, SupportStatus).
support_status0(Node, SupportStatus):-
  rdf_datatype(Node, doyle:support_status, string, SupportStatus, doyle).

%% supporting_nodes(+Node:node, -SupportingNodes:ordset(node)) is det.

supporting_nodes(Node, SupportingNodes):-
  setoff(
    SupportingNode,
    rdf(Node, doyle:has_supporting_node, SupportingNode),
    SupportingNodes
  ).

% Tests the TMS for the example that Doyle gave.
test:-
  reset_tms,
  add_node('A', A),
  add_node('B', B),
  add_node('C', C),
  add_node('D', D),
  add_node('E', E),
  add_node('F', F),
  add_justification([C],    [],  dummy, A, _),
  add_justification([],     [A], dummy, B, _),
  add_justification([A],    [],  dummy, C, _),
  add_justification([B],    [],  dummy, D, _),
  add_justification([C],    [],  dummy, D, _),
  add_justification([],     [],  dummy, E, _),
  add_justification([C, E], [],  dummy, F, _),
  export_tms.

