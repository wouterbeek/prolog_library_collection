:- module(
  fca,
  [
    a2o/3, % +Context:compound
           % +Attribute
           % -Object
    as2os/3, % +Context:compound
             % +Attributes:ordset
             % -Objects:ordset
    concept_attributes/2, % +Concept:compound
                          % -Attributes:ordset
    concept_cardinality/2, % +Concept:compound
                           % ?Cardinality:nonneg
    concept_closure/3, % +Context:atom
                       % +PartialConcept:compound
                       % -Concept:compound
    concept_components/3, % ?Concept:compound
                          % ?Objects:ordset
                          % ?Attributes:ordset
    concept_objects/2, % +Concept:compound
                       % -Objects:ordset
    concepts/2, % +Context:compound
                % -Concepts:list(compound)
    context_attributes/2, % +Context:compound
                          % ?Attributes:ordset
    context_components/4, % +Context:compound
                          % -Objects:ordset
                          % -Attributes:ordset
                          % :Mapping_2
    context_objects/2, % +Context:compound
                       % ?Objects:ordset
    fca_hasse/2, % +Context:compound
                 % -Hasse:ugraph
    fca_lattice/2, % +Context:compound
                   % -Lattice:ugraph
    is_concept/2, % +Context:compound
                  % +Concept:compound
    is_strict_subconcept/2, % +Concept1:compound
                            % +Concept2:compound
    is_subconcept/2, % +Concept1:compound
                     % +Concept2:compound
    o2a/3, % +Context:compound
           % +Object
           % -Attribute
    os2as/3, % +Context:compound
             % +Objects:ordset
             % -Attributes:ordset
    sort_concepts/2 % +Concepts:list(compound)
                    % -Sorted:list(compound)
  ]
).

/** <module> Formal Concept Analysis

A context is a tuple 〈Os,As,Goal_2〉 such that
Os is the set of objects,
As is the set of attributes,
and Goal_2 is the relation between Os and As (in that order).

@author Wouter Beek
@version 2015/10-2015/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(set/intersection)).
:- use_module(library(set/set_ext)).
:- use_module(library(thread)).





%! a2o(+Context:compound, +Attribute, -Object) is nondet.

a2o(context(_,_,Map_2), A, O):- call(Map_2, O, A).



%! as2os(+Context:compound, +Attributes:ordset, -Objects:ordset) is det.
% Map from attributes to objects exhibiting those attributes.

as2os(Con, [], Os):- !, context_objects(Con, Os).
as2os(Con, [A1|As], Os):-
  aggregate_all(set(O), (a2o(Con, A1, O), maplist(o2a(Con, O), As)), Os).



%! concept_attributes(+Concept:compound, -Attributes:ordset) is det.

concept_attributes(concept(_,As), As).



%! concept_cardinality(+Concept:compound, +Cardinality:nonneg) is semidet.
%! concept_cardinality(+Concept:compound, -Cardinality:nonneg) is det.
% The **cardinality** of a concept is the number of attributes of that concept.

concept_cardinality(concept(_,As), Card):- length(As, Card).



%! concept_closure(+Context:compound, +PartialConcept:compound, -Concept:compound) is semidet.
% Succeeds if Concept is the unique formal concept based on the given
% PartialConcept.
%
% A partial concept must contain a subset of objects or a subset of attributes.
%
% @throws instantiation_error

concept_closure(Con, concept(Os0,_), concept(Os,As)):-
  nonvar(Os0), !,
  os2as(Con, Os0, As),
  as2os(Con, As, Os).
concept_closure(Con, concept(_,As0), concept(Os,As)):-
  nonvar(As0), !,
  as2os(Con, As0, Os),
  os2as(Con, Os, As).
concept_closure(_, C0, _):-
  instantiation_error(C0).



%! concept_components(+Concept:compound, -Objects:ordset, -Attributes:ordset) is det.
%! concept_components(-Concept:compound, +Objects:ordset, +Attributes:ordset) is det.

concept_components(concept(Os,As), Os, As).



%! concept_objects(+Concept:compound, -Objects:ordset) is det.

concept_objects(concept(Os,_), Os).



%! concepts(+Context:compound, -Concepts:list(compound)) is det.

%concepts(Con, Cs):-
%  context_attributes(Con, As),
%  maplist(singleton, As, SimpleAss),
%  maplist(as2os(Con), SimpleAss, SimpleOss),
%  intersections(SimpleOss, ComplexOss),
%  context_objects(Con, Os),
%  maplist(os2as(Con), [Os|ComplexOss], ComplexAss),
%  maplist(concept_components, Cs, [Os|ComplexOss], ComplexAss).
concepts(Con, Cs):-
  context_objects(Con, Os),
  maplist(singleton, Os, SimpleOss),
  maplist(os2as(Con), SimpleOss, SimpleAss0),
  sort(SimpleAss0, SimpleAss),
  intersections(SimpleAss, ComplexAss),
  concurrent_maplist(as2os(Con), ComplexAss, ComplexOss),
  maplist(concept_components, Cs, ComplexOss, ComplexAss).



%! context_attributes(+Context:compound, +Attributes:ordset) is semidet.
%! context_attributes(+Context:compound, -Attributes:ordset) is det.

context_attributes(context(_,Attr_1,_), As):-
  aggregate_all(set(A), call(Attr_1, A), As).



%! context_components(+Context:compound, -Objects:ordset, -Attributes:ordset, :Mapping_2) is det.

context_components(Con, Os, As, Map_2):-
  Con = context(_,_,Map_2),
  context_attributes(Con, As),
  context_objects(Con, Os).



%! context_objects(+Context:compound, +Objects:ordset) is semidet.
%! context_objects(+Context:compound, -Objects:ordset) is det.

context_objects(context(Obj_1,_,_), Os):-
  aggregate_all(set(O), call(Obj_1, O), Os).



%! direct_subconcept(
%!   +Concepts:list(compound),
%!   ?Concept1:compound,
%!   ?Concept2:compound
%! ) is nondet.

direct_subconcept(Cs, A, B):-
  sort_concepts(Cs, SortedCs),
  % NONDET
  split_member(SortedCs, _, A, AfterA),
  % NONDET
  split_member(AfterA, BetweenAB, B, _),
  is_strict_subconcept(A, B),
  \+ (
    member(C, BetweenAB),
    is_strict_subconcept(A, C),
    is_strict_subconcept(C, B)
  ).



%! fca_hasse(+Context:compound, -Hasse:ugraph) is det.

fca_hasse(Con, Hasse):-
  concepts(Con, Cs),
  aggregate_all(set(C1-C2), direct_subconcept(Cs, C1, C2), Es),
  s_graph_components(Hasse, Cs, Es).



%! fca_lattice(+Context:compound, -Lattice:ugraph) is det.

fca_lattice(Con, Lattice):-
  concepts(Con, Cs),
  aggregate_all(
    set(C1-C2),
    (
      member(C1, C2, Cs),
      maplist(concept_components, [C1,C2], [OsC1,OsC2], _),
      ord_subset(OsC1, OsC2)
    ),
    Es
  ),
  s_graph_components(Lattice, Cs, Es).



%! is_concept(+Context:compound, +Concept:compound) is semidet.

is_concept(Con, C):- concept_closure(Con, C, C).



%! is_strict_subconcept(+Concept1:compound, +Concept2:compound) is semidet.

is_strict_subconcept(C, D):-
  maplist(concept_components, [C,D], _, [CAs,DAs]),
  strict_subset(CAs, DAs).



%! is_subconcept(+Concept1:compound, +Concept2:compound) is semidet.

is_subconcept(C, D):-
  maplist(concept_components, [C,D], _, [CAs,DAs]),
  subset(CAs, DAs).



%! o2a(+Context:compound, +Object, -Attribute) is nondet.

o2a(context(_,_,Map_2), O, A):- call(Map_2, O, A).



%! os2as(+Context:compound, +Objects:ordset, -Attributes:ordset) is det.

os2as(Con, [], As):- !, context_attributes(Con, As).
os2as(Con, [O1|Os], As):-
  aggregate_all(set(A), (o2a(Con, O1, A), maplist(a2o(Con, A), Os)), As).



%! sort_concepts(+Concepts:list(compound), -Sorted:list(compound)) is det.

sort_concepts(Cs, SortedCs):-
  maplist(concept_cardinality, Cs, Cards),
  pairs_keys_values(Pairs, Cards, Cs),
  keysort(Pairs, SortedPairs),
  pairs_values(SortedPairs, SortedCs).
