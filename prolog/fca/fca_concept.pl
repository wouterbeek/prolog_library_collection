:- module(
  fca_concept,
  [
    a2o/3, % +ContextName:atom
           % +Attributes:list
           % -Objects:ordset
    concept/3, % +ContextName:atom
               % +PartialConcept:compound
               % -Concept:compound
    concept_attributes/2, % +Concept:compound
                          % -Attributes:ordset
    concept_cardinality/2, % +Concept:compound
                           % ?Cardinality:nonneg
    concept_objects/2, % +Concept:compound
                       % -Objects:ordset
    is_concept/2, % +ContextName:atom
                  % +Concept:compound
    o2a/3 % +ContextName:atom
          % +Objects:list
          % -Attributes:ordset
  ]
).

/** <module> Formal Concept Analysis: Concept

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(fca/fca_context)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).





%! a2o(+ContextName:atom, +Attributes:list, -Objects:ordset) is det.
% Map from attributes to objects exhibiting those attributes.
%
% @throws existence_error in case no context with given name exists.

a2o(ContextName, _, _):-
  \+ exists_context(ContextName), !,
  existence_error(context, ContextName).
a2o(ContextName, [A1|As], Os):-
  aggregate_all(
    set(O),
    (
      % An object exhibiting the first attribute.
      fca_i(ContextName, O, A1),
      % An object exhibiting to all other attributes.
      maplist(fca_i(ContextName, O), As)
    ),
    Os
  ).



%! concept(
%!   +ContextName:atom,
%!   +PartialConcept:compound,
%!   -Concept:compound
%! ) is semidet.
% Succeeds if Concept is the unique formal concept based on the given
% PartialConcept.
%
% A partial concept must contain a subset of objects or a subset of attributes.
%
% @throws instantiation_error

concept(ContextName, concept(PartialOs,_), concept(Os,As)):-
  nonvar(PartialOs), !,
  o2a(ContextName, PartialOs, As),
  a2o(ContextName, As, Os).
concept(ContextName, concept(_,PartialAs), concept(Os,As)):-
  nonvar(PartialAs), !,
  a2o(ContextName, PartialAs, Os),
  o2a(ContextName, Os, As).
concept(_, PartialConcept, _):-
  instantiation_error(PartialConcept).



%! concept_attributes(+Concept:compound, -Attributes:ordset) is det.

concept_attributes(concept(_,As), As).



%! concept_objects(+Concept:compound, -Objects:ordset) is det.

concept_objects(concept(Os,_), Os).



%! concept_cardinality(+Concept:compound, +Cardinality:nonneg) is semidet.
%! concept_cardinality(+Concept:compound, -Cardinality:nonneg) is det.
% The **cardinality** of a concept is the number of attributes of that concept.

concept_cardinality(Concept, Card):-
  concept_attributes(Concept, As),
  length(As, Card).



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



%! is_concept(+ContextName:atom, +Concept:compound) is semidet.

is_concept(ContextName, Concept):-
  concept_components(Concept, Os, As),
  o2a(ContextName, Os, As),
  a2o(ContextName, As, Os).



%! o2a(+ContextName:atom, +Objects:list, -Attributes:ordset) is det.
% @throws existence_error

o2a(ContextName, _, _):-
  \+ exists_context(ContextName), !,
  existence_error(context, ContextName).
o2a(ContextName, [O1|Os], As):-
  aggregate_all(
    set(A),
    (
      % An attribute that is exhibited by the first object.
      fca_i(ContextName, O1, A),
      % The same attribute is exhibited by the other objects as well.
      maplist(\O^fca_i(ContextName, O, A), Os)
    ),
    As
  ).

:- begin_tests('o2a/3').

test(
  'o2a(+,+,-) is det. TRUE',
  [forall(o2a_test(ContextName,Os,As,true))]
):-
  o2a(ContextName, Os, As).
test(
  'o2a(+,+,-) is det. FALSE',
  [fail,forall(o2a_test(ContextName,Os,As,true))]
):-
  aggregate_all(set(A), context_attribute(ContextName, A), AllAs),
  ord_subset(CounterExample, AllAs),
  CounterExample \== As,
  o2a(ContextName, Os, CounterExample).

o2a_test(tab(1), [x2], [y1,y3,y4], true).
o2a_test(tab(1), [x2,x3], [y3,y4], true).
o2a_test(tab(1), [x1,x4,x5], [], true).

:- end_tests('o2a/3').
