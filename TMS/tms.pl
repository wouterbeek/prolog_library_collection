:- module(
  tms,
  [
% TMS EXPLANATIONS
    tms_argument/2, % +Node:node
                    % -Argument:ordset(justification)

% TMS REGISTRATION
    deregister_tms/2, % +Type:atom
                      % +TMS:atom
    is_registered_tms/1, % +TMS:atom
    register_tms/2, % +Type:atom
                    % +TMS:atom
    registered_tms/2, % ?Type:atom
                      % ?TMS:atom

% GENERIC TMS PREDICATES
    is_in_node/2, % +TMS:atom
                  % +Node:node
    is_justification/2, % +TMS:atom
                        % +Justification:justification
    is_node/2, % +TMS:atom
               % +Node:node
    is_out_node/2, % +TMS:atom
                   % +Node:node
    justification/2, % +TMS:atom
                     % -Justification:justification
    node/2, % +TMS:atom
            % -Node:node

% TEST PREDICATES
    doyle_test0/0,
    doyle_test1/0,
    doyle_test2/0
  ]
).

/** <module> TMS

The generic predicates for Truth-Maintenance Systems.

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(db_ext)).
:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_search)).
:- use_module(rdfs(rdfs_build)).
:- use_module(tms(doyle)).
:- use_module(tms(tms_export)).

:- dynamic(registered_tms(_Type, _TMS)).

:- rdf_register_ns(tms, 'http://www.wouterbeek.com/tms.owl#').

:- rdf_meta(tms_argument(r,-)).



% TMS EXPLANATIONS %

tms_argument(Node, Argument):-
  breadth_first_rdf_traversal(
    Node,
    tms:has_consequent,
    tms:has_antecedent,
    _Nodes,
    Argument
  ).



% TMS REGISTRATION %

deregister_tms(Type, TMS):-
  retractall(registered_tms(Type, TMS)).

generic_tms(TMS, Pred, Args):-
  once(registered_tms(Type, TMS)),
  generic(Type, Pred, Args).

is_registered_tms(TMS):-
  nonvar(TMS),
  once(registered_tms(_Type, TMS)).

register_tms(Type, TMS):-
  assert_novel(registered_tms(Type, TMS)).



% GENERIC TMS PREDICATES

is_in_node(TMS, Node):-
  generic_tms(TMS, is_in_node, [Node]).

is_justification(TMS, Justification):-
  is_registered_tms(TMS),
  nonvar(Justification),
  once(justification(TMS, Justification)).

is_node(TMS, Node):-
  is_registered_tms(TMS),
  nonvar(Node),
  once(node(TMS, Node)).

is_out_node(TMS, Node):-
  generic_tms(TMS, is_out_node, [Node]).

justification(TMS, Justification):-
  rdfs_individual_of(Justification, tms:'Justification'),
  rdf(Justification, _, _, TMS:_).

node(TMS, Node):-
  rdfs_individual_of(Node, tms:'Node'),
  rdf(Node, _, _, TMS:_).

tms_init(TMS):-
  atom(TMS),
  rdfs_assert_subproperty(tms:has_in, tms:has_antecedent, TMS),
  rdfs_assert_subproperty(tms:has_out, tms:has_antecedent, TMS).



% TEST PREDICATES FOR SPECIFIC TMS IMPLEMENTATIONS %

doyle_test0:-
  TMS = doyle_test0,
  register_tms(doyle, TMS),
  doyle_reset(TMS),
  doyle_init(TMS),
  doyle_add_node(TMS, '1', N1),
  doyle_add_justification(TMS, [], [], 'J1', N1, _J1),
  export_tms(TMS).

doyle_test1:-
  TMS = doyle_test1,
  register_tms(doyle, TMS),
  doyle_reset(TMS),
  doyle_init(TMS),
  doyle_add_node(TMS, 'A', A),
  doyle_add_node(TMS, 'B', B),
  doyle_add_node(TMS, 'C', C),
  doyle_add_node(TMS, 'D', D),
  doyle_add_node(TMS, 'E', E),
  doyle_add_node(TMS, 'F', F),
  doyle_add_justification(TMS, [C],    [],  'J1', A, _),
  doyle_add_justification(TMS, [],     [A], 'J2', B, _),
  doyle_add_justification(TMS, [A],    [],  'J3', C, _),
  doyle_add_justification(TMS, [B],    [],  'J4', D, _),
  doyle_add_justification(TMS, [C],    [],  'J5', D, _),
  doyle_add_justification(TMS, [],     [],  'J6', E, _),
  doyle_add_justification(TMS, [C, E], [],  'J7', F, _),
  export_tms(TMS).

doyle_test2:-
  TMS = doyle_test2,
  register_tms(doyle, TMS),
  doyle_reset(TMS),
  doyle_init(TMS),
  doyle_add_node(TMS, '1', N1),
  doyle_add_node(TMS, '2', N2),
  doyle_add_node(TMS, '3', N3),
  doyle_add_node(TMS, '4', N4),
  doyle_add_node(TMS, '5', N5),
  doyle_add_node(TMS, '6', N6),
  doyle_add_justification(TMS, [N3],    [],   'J1', N1, _J1 ),
  doyle_add_justification(TMS, [],      [N1], 'J2', N2, _J2 ),
  doyle_add_justification(TMS, [N1],    [],   'J3', N3, _J3 ),
  doyle_add_justification(TMS, [N2],    [],   'J4', N4, _J4a),
  doyle_add_justification(TMS, [N3],    [],   'J5', N4, _J4b),
  doyle_add_justification(TMS, [],      [],   'J6', N5, _J5 ),
  doyle_add_justification(TMS, [N3,N5], [],   'J7', N6, _J6 ),
  export_tms(TMS).

