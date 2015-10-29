:- module(
  fca_context,
  [
    assert_context/1, % +Context:compound
    context_attribute/2, % ?ContextName:atom
                         % ?Attribute
    context_attributes/2, % +ContextName:atom
                          % -Attributes:ordset
    context_name/2, % +Context:compound
                    % -ContextName:atom
    context_object/2, % ?ContextName:atom
                      % ?Object
    context_objects/2, % +ContextName:atom
                       % -Objects:ordset
    exists_context/1, % +ContextName:atom
    fca_i/3 % ?ContextName:atom
            % ?Object
            % ?Attribute
  ]
).

/** <module> Formal Concept Analysis: Context

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(pair_ext)).
:- use_module(library(semweb/rdf_db)).

%! context_attribute(?ContextName:atom, ?Attribute) is nondet.

:- dynamic(context_attribute/2).

%! fca_i(?ContextName, ?Object, ?Attribute) is nondet.

:- dynamic(fca_i/3).

fca_i(ContextName, O, A):-
  rdf_graph(ContextName), !,
  rdf(O, P, O, ContextName),
  A = popair(P,O).

%! context_object(?ContextName:atom, ?Object) is nondet.

:- dynamic(context_object/2).





%! assert_context(+Context:compound) is det.

assert_context(context(ContextName,Os,As,I)):-
  retract_context(ContextName),
  maplist(\O^assertz(context_object(ContextName,O)), Os),
  maplist(\A^assertz(context_attribute(ContextName,A)), As),
  maplist(\Pair^(pair(Pair, O, A), assertz(fca_i(ContextName,O,A))), I).



%! context_attributes(+ContextName:atom, -Attributes:ordset) is det.

context_attributes(ContextName, As):-
  aggregate_all(set(A), context_attribute(ContextName, A), As).



%! context_name(+Context:compound, -ContextName:atom) is det.

context_name(context(ContextName,_,_,_), ContextName).



%! context_objects(+ContextName:atom, -Objects:ordset) is det.

context_objects(Context, Os):-
  aggregate_all(set(O), context_object(Context, O), Os).



%! exists_context(+ContextName:atom) is semidet.
% Succeeds in case a context with the given name is currently defined.

exists_context(ContextName):-
  context_attribute(ContextName, _), !.
exists_context(ContextName):-
  context_object(ContextName, _).



%! retract_context(+ContextName:atom) is det.

retract_context(ContextName):-
  retractall(context_attribute(ContextName,_)),
  retractall(context_object(ContextName,_)),
  retractall(fca_i(ContextName,_,_)).
