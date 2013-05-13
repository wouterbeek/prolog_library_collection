:- module(
  rdf_tms,
  [
    rdf_materialize/1, % +Graphs:oneof([atom,list(atom)])
    rdf_materialize/2, % +Graphs:oneof([atom,list(atom)])
                       % -TMS:atom
    rdf_materialize_tms_test/1, % +TMS:atom
    rdf_tms/3, % ?S
               % ?P
               % ?O
    rdfs_inconsistent/1 % +Graph:atom
  ]
).

/** <module> RDF TMS

Uses a the Doyle TMS to keep track of RDF(S) materialization.

@author Wouter Beek
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_export)).
:- use_module(rdf(rdf_graph)).
:- use_module(rdf(rdf_read)).
:- use_module(tms(doyle)).
:- use_module(tms(tms)).

:- rdf_meta(rdf_axiom(r,r,r)).
:- rdf_meta(rdf_node(r,r,r,+,?)).
:- rdf_meta(rdf_rule(r,r,r,+,-,-,-)).
:- rdf_meta(rdf_test_triple(r,r,r)).
:- rdf_meta(rdf_tms(r,r,r)).

:- dynamic(bnode_literal_map0(_BNode, _Literal)).



bnode_literal_map(BNode, Lit):-
  bnode_literal_map0(BNode, Lit),
  !.
bnode_literal_map(BNode, Lit):-
  rdf_bnode(BNode),
  assert(bnode_literal_map0(BNode, Lit)).

% @tbd
rdfs_inconsistent(Graph):-
  rdf_graph(Graph),
  !.

rdf_materialize(Graphs):-
  rdf_materialize(Graphs, _TMS).

%% rdf_materialize(+Graphs:list(atom), -TMS:atom) is det.
% Performs materialization closure on the triples in the given graphs.
%
% @param Graphs A list of atomic names of RDF graphs.

rdf_materialize(Graphs, TMS):-
  % Type checking.
  maplist(rdf_graph, Graphs),
  !,

  % Start a new TMS.
  flag(materialization_graphs, TMS_ID, TMS_ID + 1),
  format(atom(TMS), 'rdf_tms_~w', [TMS_ID]),
  format(atom(MergedGraph), 'rdf_tms_~w_tmp', [TMS_ID]),
  register_tms(doyle, TMS),
  doyle_reset(TMS),
  doyle_init(TMS),

  % Merge the graphs.
  rdf_graph_merge(Graphs, MergedGraph),

  % Create an assumption node for each triple in the merged graph;
  % and assert a new triple, connecting it to the node via the node ID.
  forall(
    (
      rdf(S, P, O, MergedGraph)
    ;
      rdf_axiom(S, P, O)
    ),
    (
      (rdf_global_id(cp:'WouterBeek', S) -> gtrace ; true), %DEB
      rdf_triple_naming(S, P, O, TripleName),
      doyle_add_node(TMS, TripleName, Node),
      doyle_add_justification(TMS, [], [], 'Assumption', Node, _),
      % Connect TMS node and triple content.
      rdf_datatype(Node, tms:has_id, int, Node_ID, TMS),
      rdf_assert(S, P, O, TMS:Node_ID)
    )
  ),

  % The merged graph is not longer needed.
  rdf_unload_graph(MergedGraph),

  rdf_materialize_tms(TMS).
rdf_materialize(Graph, TMS):-
  rdf_graph(Graph),
  !,
  rdf_materialize([Graph], TMS).

%% rdf_materialize_tms(+TMS:atom) is det.
% Performs materialization closure on the triples in the given TMS.
%
% @param TMS The atomic name of a TMS that is registered with module TMS.

rdf_materialize_tms(TMS):-
  % Type checking.
  is_registered_tms(TMS),

  % Find a new triple.
  rdf_rule(S, P, O, TMS, Ins, Outs, RuleLabel),
  \+ rdf_node(S, P, O, TMS, _ExistingNode),

  % Store the new triple in a node.
  rdf_triple_naming(S, P, O, ConsequenceLabel),
  doyle_add_node(TMS, ConsequenceLabel, Consequence),

  % Connect the consequence node to an RDF triple.
  rdf_datatype(Consequence, tms:has_id, int, ID, TMS),
  rdf_assert(S, P, O, TMS:ID),
  doyle_add_justification(TMS, Ins, Outs, RuleLabel, Consequence, _J),

  % Enumerate by failure.
  fail.
% Done!
rdf_materialize_tms(_TMS).

%% rdf_materialize_tms_test(+TMS:atom) is semidet.
% Tests whether the given TMS is in accordance with the standards for
% RDF(S) semantics.
%
% @param TMS The atomic name of a TMS.

rdf_materialize_tms_test(TMS):-
  % Type checking.
  is_registered_tms(TMS),

  % The TMS must have a node for each of the test triples.
  forall(
    rdf_test_triple(S, P, O),
    rdf_node(S, P, O, TMS, _Node)
  ).

%% rdf_node(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Object:oneof([bnode,literal,uri]),
%%   +TMS:atom,
%%   ?Node:node
%% ) is nondet.

rdf_node(S, P, O, TMS, Node):-
  is_registered_tms(TMS),
  rdf(S, P, O, TMS:ID),
  rdf_datatype(Node, tms:has_id, int, ID, TMS),
  rdfs_individual_of(Node, tms:'Node').

%% rdf_rule(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Object:oneof([bnode,literal,uri]),
%%   +TMS:atom,
%%   -Ins:list(node),
%%   -Outs:list(node),
%%   -RuleLabel:atom
%% ) is nondet.

% [RDFS-12] Individuals of =|rdfs:'Datatype'|= are subclasses of
%           =|rdfs:'Literal'|=.
rdf_rule(X, rdfs:subClassOf, rdfs:'Literal', TMS, [Node], [], Label):-
  rdf_node(X, rdf:type, rdfs:'Datatype', TMS, Node),
  Label = 'Individuals of rdfs:Datatype are subclasses of rdfs:Literal.'.
% [RDFS-11] Individuals of =|rdfs:'ContainerMembershipProperty'|= are
%           subproperties of =|rdf:member|=.
rdf_rule(X, rdfs:subPropertyOf, rdfs:member, TMS, [Node], [], Label):-
  rdf_node(X, rdf:type, rdfs:'ContainerMembershipProperty', TMS, Node),
  Label =
    'Individuals of rdfs:ContainerMembershipProperty are subproperties of\c
     rdf:member.'.
% [RDFS-10b] Transitivity of subclass relation.
rdf_rule(C1, rdfs:subClassOf, C3, TMS, [Node1,Node2], [], Label):-
  rdf_node(C1, rdfs:subClassOf, C2, TMS, Node1),
  C1 \== C2,
  rdf_node(C2, rdfs:subClassOf, C3, TMS, Node2),
  C2 \== C3,
  Label = 'Transitivity of subclass relation.'.
% [RDFS-10a] Reflexivity of subclass relation.
rdf_rule(C, rdfs:subClassOf, C, TMS, [Node], [], Label):-
  rdf_node(C, rdf:type, rdfs:'Class', TMS, Node),
  Label = 'Reflexivity of subclass relation.'.
% [RDFS-9b] Individuals are closed under transitivity of the
%           subclass hierarchy.
rdf_rule(X, rdf:type, C2, TMS, [Node1,Node2], [], Label):-
  rdf_node(C1, rdfs:subClassOf, C2, TMS, Node1),
  rdf_node(X, rdf:type, C1, TMS, Node2),
  Label =
    'Individuals are closed under transitivity of the subclass hierarchy.'.
% [RDFS-9a] Resources that occur in the subclass hierarchy are
%           individuals of =|rdfs:'Class'|=.
rdf_rule(C, rdf:type, rdfs:'Class', TMS, [Node], [], Label):-
  rdf_node(C1, rdfs:subClassOf, C2, TMS, Node),
  (C = C1 ; C = C2),
  Label =
    'Resources that occur in the subclass hierarchy are individuals of\c
     rdfs:Class.'.
% [RDFS-8] Classes are subclasses of =|rdfs:'Resource'|=.
rdf_rule(C, rdfs:subClassOf, rdfs:'Resource', TMS, [Node], [], Label):-
  rdf_node(C, rdf:type, rdfs:'Class', TMS, Node),
  Label = 'Classes are subclasses of rdfs:Resource.'.
% [RDFS-7b] Use the subproperty hierarchy to derive an arbitrary triple.
rdf_rule(S, P2, O, TMS, [Node1,Node2], [], Label):-
  rdf_node(P1, rdfs:subPropertyOf, P2, TMS, Node1),
  rdf_node(S, P1, O, TMS, Node2),
  Label = 'Use the subproperty hierarchy to derive an arbitrary triple.'.
% [RDFS-7a] Resources that occur in the subproperty relation are
%           individuals of =|rdf:'Property'|=.
rdf_rule(P, rdf:type, rdf:'Property', TMS, [Node], [], Label):-
  rdf_node(P1, rdfs:subPropertyOf, P2, TMS, Node),
  (P = P1 ; P = P2),
  Label =
    'Resources that occur in the subproperty relation are individuals of\c
     rdf:Property.'.
% [RDFS-6a] Transitivity of subproperty relation.
rdf_rule(X, rdfs:subPropertyOf, Z, TMS, [Node1,Node2], [], Label):-
  rdf_node(X, rdfs:subPropertyOf, Y, TMS, Node1),
  X \== Y,
  rdf_node(Y, rdfs:subPropertyOf, Z, TMS, Node2),
  Y \== Z,
  Label = 'Transitivity of subproperty relation.'.
% [RDFS-6a] Reflexivity of subproperty relation.
rdf_rule(X, rdfs:subPropertyOf, X, TMS, [Node], [], Label):-
  rdf_node(X, rdf:type, rdfs:'Property', TMS, Node),
  Label = 'Reflexivity of subproperty relation.'.
% [RDFS-5] If <P,rdfs:range,R> and <X,P,Y>, then <Y,rdf:type,R>.
rdf_rule(Y, rdf:type, R, TMS, [Node1,Node2], [], Label):-
  rdf_node(P, rdfs:range, R, TMS, Node1),
  rdf_node(_, P, Y, TMS, Node2),
  Label = 'If <P,rdfs:range,R> and <X,P,Y>, then <Y,rdf:type,R>.'.
% [RDFS-4] If <P,rdfs:domain,D> and <X,P,Y>, then <X,rdf:type,D>.
rdf_rule(X, rdf:type, D, TMS, [Node1,Node2], [], Label):-
  rdf_node(P, rdfs:domain, D, TMS, Node1),
  rdf_node(X, P, _, TMS, Node2),
  Label = 'If <P,rdfs:domain,D> and <X,P,Y>, then <X,rdf:type,D>.'.
/* LITERALS IN SUBJECT POSITION USING BLANK NODES!
% [RDFS-3] Plain literals are individuals of =|rdfs:'Literal'|=.
rdf_rule(Lit, rdf:type, rdfs:'Literal', TMS, [Node], [], Label):-
  rdf_node(_, _, literal(Lit), TMS, Node),
  Label = 'Plain literals are individuals of rdfs:Literal.'.
*/
% [RDFS-2] Everything is an =|rdfs:'Resource'|=.
rdf_rule(X, rdf:type, rdfs:'Resource', TMS, [Node], [], Label):-
  rdf_node(S, P, O, TMS, Node),
  (X = S ; X = P ; X = O),
% @tbd LITERALS IN SUBJECT POSITION USING BLANK NODES!
\+ rdf_is_bnode(X),
  Label = 'Everything is an rdfs:Resource.'.
% [RDFS-1] If a resource has in instance, then it must be an
% =|rdfs:'Class'|=.
rdf_rule(Y, rdf:type, rdfs:'Class', TMS, [Node], [], Label):-
  rdf_node(_, rdf:type, Y, TMS, Node),
  Label = 'If a resource has in instance, then it must be an rdfs:Class.'.
% [RDF-1] Terms that occur in the predicate position are instances of
%         =|rdf:'Property'|=.
rdf_rule(X, rdf:type, rdf:'Property', TMS, [Node], [], Label):-
  rdf_node(_, X, _, TMS, Node),
  Label =
    'Terms that occur in the predicate position are instances of\c
     rdf:Property.'.
/* LITERALS IN SUBJECT POSITION USING BLANK NODES!
% [RDF-2] XML literals are instances of =|rdf:'XMLLiteral'|=.
rdf_rule(BNode, rdf:type, rdf:'XMLLiteral', TMS, [Node], [], Label):-
  rdf_global_id(rdf:'XMLLiteral', Type),
  rdf_node(_, _, literal(type(Type, Lit)), TMS, Node),
  bnode_literal_map(BNode, Lit),
  Label = 'XML literals are instances of rdf:XMLLiteral.'.
*/

rdf_tms(S, P, O):-
  rdf(S, P, O),
  once((
    registered_tms(_Type, TMS),
    rdf_node(S, P, O, TMS, N),
    tms_export:export_argument(N)
  )).

% [RDF-3] RDF axiomatic triples.
rdf_axiom(rdf:type, rdf:type, rdf:'Property').
rdf_axiom(rdf:subject, rdf:type, rdf:'Property').
rdf_axiom(rdf:predicate, rdf:type, rdf:'Property').
rdf_axiom(rdf:object, rdf:type, rdf:'Property').
rdf_axiom(rdf:first, rdf:type, rdf:'Property').
rdf_axiom(rdf:rest, rdf:type, rdf:'Property').
rdf_axiom(rdf:value, rdf:type, rdf:'Property').
rdf_axiom(rdf:'_1', rdf:type, rdf:'Property').
rdf_axiom(rdf:'_2', rdf:type, rdf:'Property').
rdf_axiom(rdf:'_3', rdf:type, rdf:'Property').
/*
rdf_axiom(UriRef, rdf:type, rdf:'Property'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
*/
rdf_axiom(rdf:nil, rdf:type, rdf:'List').
% [RDFS-13a] RDFS axiomatic triples: domains.
rdf_axiom(rdf:type, rdfs:domain, rdfs:'Resource').
rdf_axiom(rdfs:domain, rdfs:domain, rdf:'Property').
rdf_axiom(rdfs:range, rdfs:domain, rdf:'Property').
rdf_axiom(rdfs:subPropertyOf, rdfs:domain, rdf:'Property').
rdf_axiom(rdfs:subClassOf, rdfs:domain, rdfs:'Class').
rdf_axiom(rdf:subject, rdfs:domain, rdf:'Statement').
rdf_axiom(rdf:predicate, rdfs:domain, rdf:'Statement').
rdf_axiom(rdf:object, rdfs:domain, rdf:'Statement').
rdf_axiom(rdfs:member, rdfs:domain, rdfs:'Resource').
rdf_axiom(rdf:first, rdfs:domain, rdf:'List').
rdf_axiom(rdf:rest, rdfs:domain, rdf:'List').
rdf_axiom(rdfs:seeAlso, rdfs:domain, rdfs:'Resource').
rdf_axiom(rdfs:isDefinedBy, rdfs:domain, rdfs:'Resource').
rdf_axiom(rdfs:comment, rdfs:domain, rdfs:'Resource').
rdf_axiom(rdfs:label, rdfs:domain, rdfs:'Resource').
rdf_axiom(rdf:value, rdfs:domain, rdfs:'Resource').
% [RDFS-13b] RDFS axiomatic triples: range.
rdf_axiom(rdf:type, rdfs:range, rdfs:'Class').
rdf_axiom(rdfs:domain, rdfs:range, rdfs:'Class').
rdf_axiom(rdfs:range, rdfs:range, rdfs:'Class').
rdf_axiom(rdfs:subPropertyOf, rdfs:range, rdf:'Property').
rdf_axiom(rdfs:subClassOf, rdfs:range, rdfs:'Class').
rdf_axiom(rdf:subject, rdfs:range, rdfs:'Resource').
rdf_axiom(rdf:predicate, rdfs:range, rdfs:'Resource').
rdf_axiom(rdf:object, rdfs:range, rdfs:'Resource').
rdf_axiom(rdfs:member, rdfs:range, rdfs:'Resource').
rdf_axiom(rdf:first, rdfs:range, rdfs:'Resource').
rdf_axiom(rdf:rest, rdfs:range, rdf:'List').
rdf_axiom(rdfs:seeAlso, rdfs:range, rdfs:'Resource').
rdf_axiom(rdfs:isDefinedBy, rdfs:range, rdfs:'Resource').
rdf_axiom(rdfs:comment, rdfs:range, rdfs:'Literal').
rdf_axiom(rdfs:label, rdfs:range, rdfs:'Literal').
rdf_axiom(rdf:value, rdfs:range, rdfs:'Resource').
% [RDFS-13c] RDFS axiomatic triples: subclass hierarchy.
rdf_axiom(rdf:'Alt', rdfs:subClassOf, rdfs:'Container').
rdf_axiom(rdf:'Bag', rdfs:subClassOf, rdfs:'Container').
rdf_axiom(rdf:'Seq', rdfs:subClassOf, rdfs:'Container').
rdf_axiom(rdfs:'ContainerMembershipProperty', rdfs:subClassOf, rdf:'Property').
% [RDFS-13d] RDFS axiomatic triples: subproperty hierarchy.
rdf_axiom(rdfs:isDefinedBy, rdfs:subPropertyOf, rdfs:seeAlso).
% [RDFS-13e] RDFS axiomatic triples: datatypes.
rdf_axiom(rdf:'XMLLiteral', rdf:type, rdfs:'Datatype').
rdf_axiom(rdf:'XMLLiteral', rdfs:subClassOf, rdfs:'Literal').
rdf_axiom(rdfs:'Datatype', rdfs:subClassOf, rdfs:'Class').
% [RDFS-13f] RDFS axiomatic triples: container membership properies.
rdf_axiom(rdf:'_1', rdf:type, rdfs:'ContainerMembershipProperty').
rdf_axiom(rdf:'_1', rdfs:domain, rdfs:'Resource').
rdf_axiom(rdf:'_1', rdfs:range, rdfs:'Resource').
rdf_axiom(rdf:'_2', rdf:type, rdfs:'ContainerMembershipProperty').
rdf_axiom(rdf:'_2', rdfs:domain, rdfs:'Resource').
rdf_axiom(rdf:'_2', rdfs:range, rdfs:'Resource').
rdf_axiom(rdf:'_3', rdf:type, rdfs:'ContainerMembershipProperty').
rdf_axiom(rdf:'_3', rdfs:domain, rdfs:'Resource').
rdf_axiom(rdf:'_3', rdfs:range, rdfs:'Resource').
/*
rdfax(UriRef, rdf:type, rdfs:'ContainerMembershipProperty'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
rdfax(UriRef, rdfs:domain, rdfs:'Resource'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
rdfax(UriRef, rdfs:range, rdfs:'Resource'):-
  between(1, inf, Integer),
  format(atom(Local), '_~w', [Integer]),
  rdf_global_id(rdf:Local, UriRef).
*/

%% rdf_test_triple(
%%   ?Subject:oneof([bnode,uri]),
%%   ?Predicate:uri,
%%   ?Object:oneof([bnode,literal,uri])
%% ) is nondet.
% There triples should be part of the materialization of any RDFS graph.
% These triples must be present in a rdf_materialized TMS.

rdf_test_triple(rdfs:'Resource', rdf:type, rdfs:'Class').
rdf_test_triple(rdfs:'Class', rdf:type, rdfs:'Class').
rdf_test_triple(rdfs:'Literal', rdf:type, rdfs:'Class').
rdf_test_triple(rdf:'XMLLiteral', rdf:type, rdfs:'Class').
rdf_test_triple(rdfs:'Datatype', rdf:type, rdfs:'Class').
rdf_test_triple(rdf:'Seq', rdf:type, rdfs:'Class').
rdf_test_triple(rdf:'Bag', rdf:type, rdfs:'Class').
rdf_test_triple(rdf:'Alt', rdf:type, rdfs:'Class').
rdf_test_triple(rdfs:'Container', rdf:type, rdfs:'Class').
rdf_test_triple(rdf:'List', rdf:type, rdfs:'Class').
rdf_test_triple(rdfs:'ContainerMembershipProperty', rdf:type, rdfs:'Class').
rdf_test_triple(rdf:'Property', rdf:type, rdfs:'Class').
rdf_test_triple(rdf:'Statement', rdf:type, rdfs:'Class').
rdf_test_triple(rdfs:domain, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:range, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:subPropertyOf, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:subClassOf, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:member, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:seeAlso, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:isDefinedBy, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:comment, rdf:type, rdf:'Property').
rdf_test_triple(rdfs:label, rdf:type, rdf:'Property').

test1:-
  Graph = rdf_tms_test,
  rdf_assert(rdf:a, rdf:b, rdf:c, Graph),
  rdf_materialize(Graph, TMS),
  tms_export:export_tms(TMS),
  %rdfs_individual_of(N, tms:'Node'),
  %rdf_datatype(N, tms:has_id, int, 103, TMS),
  %tms_export:export_argument(N),
  true.
