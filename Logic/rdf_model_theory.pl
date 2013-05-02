:- module(
  rdf_model_theory,
  [
    clear_model/0,
    load_model/1, % +Name:atom
    print_model/0,
    satisfy/2 % +X:oneof([atom,list(triple),triple])
              % -Assignments:ord_set(assignment)
  ]
).

/** <module> RDF MODEL THEORY

The model theory for RDF.

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(db_ext)).
:- use_module(library(ordsets)).
:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_graph)).

:- dynamic(i_ext(_Object, _Pairs)).
:- dynamic(i_l(_Pair, _Object)).
:- dynamic(i_s(_UriRef, _Object)).
:- dynamic(literal_value(_Object)).
:- dynamic(property(_Object)).
:- dynamic(resource(_Object)).
:- dynamic(typed_literal(_Pair)).

:- rdf_meta(add_property(r,+,+)).
:- rdf_meta(add_resource(r,+)).
:- rdf_meta(i(r,-,+,-)).

:- rdf_register_prefix(ex, 'http://www.example.com/').



add_literal_value(Object):-
  assert_novel(literal_value(Object)).

% Function *IS* maps URI resources to property objects.
add_property(UriRef, Object, Pairs):-
  assert_novel(i_ext(Object, Pairs)),
  assert_novel(i_s(UriRef, Object)),
  assert_novel(property(Object)),
  forall(
    member([Former,Latter], Pairs),
    maplist(add_resource, [Former,Latter])
  ).

add_resource(Object):-
  assert_novel(resource(Object)).

% Function *IS* maps URI resources to resource objects.
add_resource(UriRef, Object):-
  assert_novel(i_s(UriRef, Object)),
  add_resource(Object).

% Function *IL* maps typed literal values onto resources.
add_typed_literal(Type, Value, Object):-
  assert_novel(i_l([Type,Value], Object)),
  assert_novel(resource(Object)),
  assert_novel(typed_literal([Type,Value])).

clear_model:-
  retractall(i_ext(_Object1, _Pair1)),
  retractall(i_l(_Pair2, _Object2)),
  retractall(i_s(_UriRef3, _Object3)),
  retractall(literal_value(_Object4)),
  retractall(property(_Object5)),
  retractall(resource(_Object6)),
  retractall(typed_literal(_Pair7)).

load_model(test1):-
  add_property(ex:a, 1, [[1,2],[2,1]]),
  add_property(ex:b, 1, [[1,2],[2,1]]),
  add_resource(ex:c, 2),
  add_typed_literal(ex:b, whatever, 2).

print_model:-
  print_objects,
  print_properties,
  print_urirefs,
  print_typed_literals.

print_objects:-
  findall(
    ResourceAtom,
    (
      resource(Resource),
      format(atom(ResourceAtom), '~w', [Resource])
    ),
    ResourceAtoms
  ),
  atomic_list_concat(ResourceAtoms, ',', ResourcesAtom),
  format(user_output, 'RESOURCES: {~w}\n', [ResourcesAtom]).

print_properties:-
  format(user_output, 'PROPERTIES:\n', []),
  forall(
    property(Object),
    (
      i_ext(Object, Pairs),
      findall(
        PairAtom,
        (
          member([Former,Latter], Pairs),
          format(atom(PairAtom), '<~w,~w>', [Former,Latter])
        ),
        PairAtoms
      ),
      atomic_list_concat(PairAtoms, ',', PairsAtom),
      format(user_output, '\t~w\t->\t{~w}\n', [Object, PairsAtom])
    )
  ).

print_typed_literals:-
  format(user_output, 'TYPED LITERALS:\n', []),
  forall(
    i_l([Type,Value], Object),
    format(user_output, '\t"~w"^^~w\t->\t~w\n', [Value,Type, Object])
  ).

print_urirefs:-
  format(user_output, 'IS:\n', []),
  forall(
    i_s(UriRef, Object),
    format(user_output, '\t~w\t->\t~w\n', [UriRef, Object])
  ).

% Literal values are resources.
resource(Object):-
  literal_value(Object).

satisfy(X, G):-
  satisfy(X, [], G).

% Satisfaction for an RDF graph.
satisfy(Graph, G1, G2):-
  catch(rdf_graph(Graph), _, fail), !,
  rdf_graph_triples(Graph, Triples),
  satisfy(Triples, G1, G2).
% Satisfaction for a set of RDF triples.
satisfy(Triples, G1, Gn):-
  is_list(Triples),!,
  foldl(satisfy, Triples, G1, Gn).
% Satisfaction for an RDF triple.
satisfy(rdf(STerm1, PTerm1, OTerm1), G1, G4):-
  maplist(
    rdf_global_object,
    [STerm1, PTerm1, OTerm1],
    [STerm2, PTerm2, OTerm2]
  ),
  i(STerm2, S, G1, G2),
  i(PTerm2, P, G2, G3),
  i(OTerm2, O, G3, G4),
  i_ext(P, Pairs),
  member([S, O], Pairs).

% A Prolog variable that has already been replaced by a blank node.
i(Var1, Object, G1, G2):-
  var(Var1),
  member([Var2,BNode], G1),
  % We must match the exact variable (not instantiate).
  Var2 == Var1,
  i(BNode, Object, G1, G2).
% Prolog variables are replaced with new blank nodes.
i(Var1, Object, G1, G3):-
  var(Var1),
  rdf_bnode(BNode),
  % We can only register this variable if it has no other map in the
  % assignment function yet.
  \+ (member([Var2,_], G1), Var2 == Var1),
  ord_add_element(G1, [Var2,BNode], G2),
  i(BNode, Object, G2, G3).
% Plain literal with language tag.
i(literal(lang(Lang,Plain)), [Plain,Lang], G, G):- !,
  literal_value(Plain).
% Typed literal.
i(literal(type(Type,Value)), Resource, G, G):- !,
  i_l([Type,Value], Resource).
% Plain literal without language tag.
i(literal(Plain), Plain, G, G):- !,
  literal_value(Plain).
% Blank node with existing mapping in assignment function.
i(BNode, Object, G, G):-
  rdf_is_bnode(BNode),
  member([Var, Object], G),
  % We must match the exact variable (not instantiate).
  Var == BNode.
% Blank node added to assignment function.
i(BNode, Object, G1, G2):-
  rdf_is_bnode(BNode),
  % We can only register this blank node if it has no other map in the
  % assignment function yet.
  \+ (member([Var,_], G1), Var == BNode),
  ord_add_element(G1, [BNode,Object], G2).
% URI resources are mapped to an object directly.
i(UriRef, Object, G, G):-
  % Checking for resources is not that good.
  rdf_is_resource(UriRef), \+ rdf_is_bnode(UriRef), !,
  i_s(UriRef, Object).

