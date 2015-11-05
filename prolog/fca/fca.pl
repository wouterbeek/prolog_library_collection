:- module(
  fca,
  [
    a2o/3, % +Context:compound
           % +Attribute
           % -Object
    as2os/3, % +Context:compound
             % +Attributes:ordset
             % -Objects:ordset
    concept_cardinality/2, % +Concept:compound
                           % ?Cardinality:nonneg
    concept_closure/3, % +Context:atom
                       % +PartialConcept:compound
                       % -Concept:compound
    concept_components/3, % ?Concept:compound
                          % ?Objects:ordset
                          % ?Attributes:ordset
    concepts/2, % +Context:compound
                % -Concepts:list(compound)
    context_components/4, % ?Context:compound
                          % ?Objects:ordset
                          % ?Attributes:ordset
                          % :Goal_2
    direct_subconcept/3, % +Concepts:list(compound)
                         % ?Concept1:compound
                         % ?Concept2:compound
    fca_hasse/2, % +Context:compound
                 % -Hasse:ugraph
    fca_lattice/2, % +Context:compound
                   % -Lattice:ugraph
    is_concept/2, % +Context:compound
                  % +Concept:compound
    o2a/3, % +Context:compound
           % +Object
           % -Attribute
    os2as/3, % +Context:compound
             % +Objects:ordset
             % -Attributes:ordset
    sort_concepts/2, % +Concepts:list(compound)
                     % -Sorted:list(compound)
    strict_subconcept/2 % +Concept1:compound
                        % +Concept2:compound
  ]
).

/** <module> Formal Concept Analysis

A context is a tuple 〈Os,As,Goal_2〉 such that
Os is the set of objects,
As is the set of attributes,
and Goal_2 is the relation between Os and As (in that order).

@author Wouter Beek
@version 2015/10-2015/11
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





%! a2o(+Context:compound, +Attribute, -Object) is nondet.

a2o(context(_,_,Goal_2), A, O):-
  call(Goal_2, O, A).



%! as2os(+Context:compound, +Attributes:ordset, -Objects:ordset) is det.
% Map from attributes to objects exhibiting those attributes.

as2os(context(Os,_,_), [], Os):- !.
as2os(Context, [A1|As], Os):-
  aggregate_all(
    set(O),
    (a2o(Context, A1, O), maplist(o2a(Context, O), As)),
    Os
  ).



%! concept_cardinality(+Concept:compound, +Cardinality:nonneg) is semidet.
%! concept_cardinality(+Concept:compound, -Cardinality:nonneg) is det.
% The **cardinality** of a concept is the number of attributes of that concept.

concept_cardinality(concept(_,As), Card):-
  length(As, Card).



%! concept_closure(
%!   +Context:compound,
%!   +PartialConcept:compound,
%!   -Concept:compound
%! ) is semidet.
% Succeeds if Concept is the unique formal concept based on the given
% PartialConcept.
%
% A partial concept must contain a subset of objects or a subset of attributes.
%
% @throws instantiation_error

concept_closure(Context, concept(Os0,_), concept(Os,As)):-
  nonvar(Os0), !,
  os2as(Context, Os0, As),
  as2os(Context, As, Os).
concept(Context, concept(_,As0), concept(Os,As)):-
  nonvar(As0), !,
  as2os(Context, As0, Os),
  os2as(Context, Os, As).
concept(_, C0, _):-
  instantiation_error(C0).



%! concept_components(
%!   +Concept:compound,
%!   -Objects:ordset,
%!   -Attributes:ordset
%! ) is det.
%! concept_components(
%!   -Concept:compound,
%!   +Objects:ordset,
%!   +Attributes:ordset
%! ) is det.

concept_components(concept(Os,As), Os, As).



%! concepts(+Context:compound, -Concepts:list(compound)) is det.

concepts(Context, Cs):-
  context_components(Context, _, As, _),
  maplist(singleton, As, Ass0),
  maplist(as2os(Context), Ass0, Oss0),
  intersections(Oss0, Oss),
  maplist(os2as(Context), Oss, Ass),
  maplist(concept_components, Cs, Oss, Ass).



%! context_components(
%!   +Context:compound,
%!   -Objects:ordset,
%!   -Attributes:ordset,
%!   :Goal_2
%! ) is det.
%! context_components(
%!   -Context:compound,
%!   +Objects:ordset,
%!   +Attributes:ordset,
%!   :Goal_2
%! ) is det.

context_components(context(Os,As,Goal_2), Os, As, Goal_2).



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
  strict_subconcept(A, B),
  \+ (member(C, BetweenAB), strict_subconcept(A, C), strict_subconcept(C, B)).



%! fca_hasse(+Context:compound, -Hasse:ugraph) is det.

fca_hasse(Context, Hasse):-
  concepts(Context, Cs),
  aggregate_all(set(C1-C2), direct_subconcept(Cs, C1, C2), Es),
  s_graph_components(Hasse, Cs, Es).



%! fca_lattice(+Context:compound, -Lattice:ugraph) is det.

fca_lattice(Context, Lattice):-
  concepts(Context, Cs),
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

is_concept(Context, C):-
  concept_closure(Context, C, C).



%! o2a(+Context:compound, +Object, -Attribute) is nondet.

o2a(context(_,_,Goal_2), O, A):-
  call(Goal_2, O, A).



%! os2as(+Context:compound, +Objects:ordset, -Attributes:ordset) is det.

os2as(context(_,As,_), [], As):- !.
os2as(Context, [O1|Os], As):-
  aggregate_all(
    set(A),
    (o2a(Context, O1, A), maplist(a2o(Context, A), Os)),
    As
  ).



%! sort_concepts(+Concepts:list(compound), -Sorted:list(compound)) is det.

sort_concepts(Cs, SortedCs):-
  maplist(concept_cardinality, Cs, Cards),
  pairs_keys_values(Pairs, Cards, Cs),
  keysort(Pairs, SortedPairs),
  pairs_values(SortedPairs, SortedCs).



%! strict_subconcept(+Concept1:compound, +Concept2:compound) is semidet.

strict_subconcept(C, D):-
  maplist(concept_components, [C,D], _, [CAs,DAs]),
  strict_subset(CAs, DAs).
