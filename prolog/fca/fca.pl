:- module(
  fca,
  [
    a2o/3, % +Context:compound
           % +Attributes:ordset
           % -Objects:ordset
    concept_closure/3, % +Context:atom
                       % +PartialConcept:compound
                       % -Concept:compound
    concept_components/3, % ?Concept:compound
                          % ?Objects:ordset
                          % ?Attributes:ordset
    concept_cardinality/2, % +Concept:compound
                           % ?Cardinality:nonneg
    context_components/4, % ?Context:compound
                          % ?Objects:ordset
                          % ?Attributes:ordset
                          % :Goal_2
    is_concept/2, % +Context:compound
                  % +Concept:compound
    o2a/3 % +Context:compound
          % +Objects:ordset
          % -Attributes:ordset
  ]
).

/** <module> Formal Concept Analysis

A context is a tuple 〈Os,As,Goal_2〉 such that
Os is the set of objects,
As is the set of attributes,
and Goal_2 is the relation between Os and As (in that order).

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).





%! a2o(+Context:compound, +Attribute, -Object) is nondet.

a2o(context(_,_,Goal_2), A, O):-
  call(Goal_2, O, A).



%! as2os(+Context:compound, +Attributes:ordset, -Objects:ordset) is det.
% Map from attributes to objects exhibiting those attributes.

as2os(context(Os,_,_), [], Os):- !.
as2os(Context, [A1|As], Os):-
  aggregate_all(
    set(O),
    (
      % An object exhibiting the first attribute.
      a2o(Context, A1, O),
      % An object exhibiting to all other attributes.
      maplist(o2a(Context, O), As)
    ),
    Os
  ).



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



%! concept_cardinality(+Concept:compound, +Cardinality:nonneg) is semidet.
%! concept_cardinality(+Concept:compound, -Cardinality:nonneg) is det.
% The **cardinality** of a concept is the number of attributes of that concept.

concept_cardinality(concept(_,As), Card):-
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

concept_attributes(concept(Os,As), Os, As).



%! is_concept(+Context:compound, +Concept:compound) is semidet.

is_concept(Context, C):-
  concept_closure(C, C).



%! o2a(+Context:compound, +Object, -Attribute) is nondet.

o2a(context(_,_,Goal_2), O, A):-
  call(Goal_2, O, A).



%! os2as(+Context:compound, +Objects:ordset, -Attributes:ordset) is det.

os2as(Context, [O1|Os], As):-
  aggregate_all(
    set(A),
    (
      % An attribute that is exhibited by the first object.
      o2a(Context, O1, A),
      % The same attribute is exhibited by the other objects as well.
      maplist(a2o(Context, A), Os)
    ),
    As
  ).

:- begin_tests('o2a/3').

test(
  'o2a(+,+,-) is det. TRUE',
  [forall(o2a_test(Context,Os,As,true))]
):-
  o2a(Context, Os, As).
test(
  'o2a(+,+,-) is det. FALSE',
  [fail,forall(o2a_test(Context,Os,As,true))]
):-
  aggregate_all(set(A), context_attribute(Context, A), AllAs),
  ord_subset(CounterExample, AllAs),
  CounterExample \== As,
  o2a(Context, Os, CounterExample).

o2a_test(tab(1), [x2], [y1,y3,y4], true).
o2a_test(tab(1), [x2,x3], [y3,y4], true).
o2a_test(tab(1), [x1,x4,x5], [], true).

:- end_tests('o2a/3').



%! o2a0(+Context:compound, ?Object, ?Attribute) is nondet.

o2a0(context(_,_,Goal_2), O, A):-
  call(Goal_2, O, A).
