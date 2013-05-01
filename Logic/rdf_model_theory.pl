:- model(
  rdf_model_theory,
  [
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

:- dyanmic(iext(_Object, _Pairs)).
:- dynamic(literal_value(_Object)).
:- dynamic(property(_Object)).
:- dynamic(resource(_Object)).
:- dynamic(typed_literal(_Pair)).

:- rdf_meta(add_resource(r,+)).



add_literal_value(Object):-
  assert_novel(literal_value(Object)).

% Function *IS* maps URI resources to property objects.
add_property(UriRef, Object, Pairs):-
  assert_novel(iext(Object, Pairs)),
  assert_novel(is(UriRef, Object)),
  assert_novel(property(Object)).
% Function *IS* maps URI resources to resource objects.
add_resource(UriRef, Object):-
  assert_novel(is(UriRef, Object)),
  assert_novel(resource(Object)).

% Function *IL* maps typed literal values onto resources.
add_typed_literal(Type, Value, Object):-
  assert_novel(il([Type,Value], Object)),
  assert_nvoel(resource(Object)),
  assert_novel(typed_literal([Type,Value])).

load_model(test1):-
  rdf_register_prefix(ex, 'http://www.example.com/'),
  add_property(ex:a, 1, [[1,2],[2,1]]),
  add_property(ex:b, 1, [[1,2],[2,1]]),
  add_resource(ex:c, 2),
  add_typed_literal(ex:b, whatever, 2).

% Literal values are resources.
resource(Object):-
  literal_value(Object).

satisfy(X, G):-
  satisfy(X, [], G).

% Satisfaction for an RDF graph.
satisfy(Graph, G1, G2):-
  rdf_graph(Graph),!,
  rdf_graph_triples(Graph, Triples),
  satisfy(Triples, G1, G2).
% Satisfaction for a set of RDF triples.
satisfy(Triples, G1, Gn):-
  is_list(Triples),!,
  foldl(satisfy, Triples, G1, Gn).
% Satisfaction for an RDF triple.
satisfy(rdf(STerm, PTerm, OTerm), G1, G4):-
  i(STerm, S, G1, G2),
  i(PTerm, P, G2, G3),
  i(OTerm, O, G3, G4),
  iext(P, Pairs),
  member([S, O], Pairs).

i(literal(lang(Lang,Plain)), [Plain,Lang], G, G):- !,
  literal_value(Plain).
i(literal(type(Type,Value)), Resource, G, G):- !,
  il([Type,Value], Resource).
i(literal(Plain), Plain, G, G):- !,
  literal_value(Plain).
i(UriRef, Object, G, G):-
  rdf_is_resource(UriRef),!,
  is(UriRef, Object).
i(BNode, Object, G, G):-
  rdf_is_bnode(BNode),
  member([BNode, Object], G).
i(BNode, Object, G, NewG):-
  rdf_is_bnode(BNode),
  ord_add_element(G, [BNode,Object], NewG). 
