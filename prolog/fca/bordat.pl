:- module(
  bordat,
  [
    generate_hasse/1 % +ContextName:atom
  ]
).

/** <module> Bordat

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(fca/fca_concept)).
:- use_module(library(fca/fca_context)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

%! concept(?ContextName:atom, ?Concept:compound) is nondet.

:- dynamic(concept/2).

%! edge(?ContextName:atom, ?Parent:compound, ?Child:compound) is nondet.

:- dynamic(edge/3).

%! parent(?ContextName:atom, ?Parent:compound) is nondet.

:- dynamic(parent/2).





%! generate_hasse(+ContextName:atom) is det.

generate_hasse(ContextName):-
  retract_hasse(ContextName),
  context_attributes(ContextName, As),
  powerset(As, PowersetAs),
  generate_hasse(ContextName, As, PowersetAs).



%! generate_hasse(
%!   +ContextName:atom,
%!   +Attributes:ordset,
%!   +PowersetAttributes:ordset(ordset)
%! ) is det.

% Done!
generate_hasse(_, _, []):- !.
% New parent concept: calculate its child concepts.
generate_hasse(ContextName, As, [ParentAs|ParentsAs]):-
  % Make sure the given subset of attributes denotes a parent concept.
  PartialConcept = concept(_,ParentAs),
  concept(ContextName, PartialConcept, Parent),
  \+ parent(ContextName, Parent), !,
  assert(parent(ContextName, Parent)),

  generate_children(ContextName, As, ParentAs, Children1),

  % Exclude the parent itself from the children, if it appears.
  exclude(==(Parent), Children1, Children2),

  % Only children that were not previously generated
  % are asserted and added to the processing stack.
  exclude(concept(ContextName), Children2, NewChildren),
  maplist(assert_concept(ContextName), [Parent|NewChildren]),

  % For all children an edge with the parent is asserted.
  maplist(assert_edge(ContextName, Parent), Children2),

  generate_hasse(ContextName, As, ParentsAs).
% Not a new parent concept: simply skip.
generate_hasse(ContextName, As, [_|ParentsAs]):-
  generate_hasse(ContextName, As, ParentsAs).



%! generate_children(
%!   +ContextName:atom,
%!   +Attributes:ordset,
%!   +ParentAttributes:ordset,
%!   -Children:list(compound)
%! ) is det.

generate_children(ContextName, As, ParentAs, Children):-
  ord_subtract(As, ParentAs, NewAs),
  findall(
    Child,
    (
      member(NewA, NewAs),
      % Find the minimal extension to the parent's attribute set
      % that respects the complete pair property.
      ord_add_element(ParentAs, NewA, PartialChildAs),
      a2o(ContextName, PartialChildAs, ChildOs),
      o2a(ContextName, ChildOs, ChildAs),
      Child = concept(ChildOs,ChildAs)
    ),
    Children
  ).





% HELPERS %

%! assert_concept(+ContextName:atom, +Concept:compound) is det.

assert_concept(ContextName, Concept):-
  assertz(concept(ContextName, Concept)).



%! assert_edge(+ContextName:atom, +Parent:compound, +Child:compound) is det.

assert_edge(ContextName, Parent, Child):-
  assertz(edge(ContextName, Child, Parent)).



%! retract_hasse(+ContextName:atom) is det.

retract_hasse(ContextName):-
  retractall(concept(ContextName, _)),
  retractall(edge(ContextName, _, _)),
  retractall(parent(ContextName, _)).
