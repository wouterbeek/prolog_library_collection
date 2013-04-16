:- module(
  error_web,
  [
    catch_web/2, % :Goal
                 % -Markup:dom
    error_web/2, % +Error:error
                 % -Markup:dom
    error_web/3 % +Format:atom
                % +Arguments:list(term)
                % -Markup:dom
  ]
).

/** <module> Error Web

Web front-end for reporting on the various errors that might occur.

---+ Exception format

==
error(
  ErrorType(ErrorSubtype, ErrorTerm),
  context(Predicates, ContextMessage)
)
==

@author Wouter Beek
@version 2012/12, 2013/02
*/

:- meta_predicate(catch_web(1,-)).



catch_web(Goal, Markup):-
  catch(
    call(Goal, Markup),
    Catcher,
    error_web(Catcher, Markup)
  ).

%% error_context_web(+Context:compound, -Markup:list) is det.
% Returns markup for the given error context.

error_context_web(Context, [element(p, [], ['No context provided.'])]):-
  var(Context),
  !.
error_context_web(context(Predicates, ContextMessage), Markup):-
  is_list(Predicates),
  !,
  maplist(predicate_to_atom, Predicates, PredicateAtoms),
  atomic_list_concat(PredicateAtoms, ' -> ', PredicateAtom),
  error_context_web(PredicateAtom, ContextMessage, Markup).
error_context_web(context(Predicate, ContextMessage), Markup):-
  !,
  error_context_web(context([Predicate], ContextMessage), Markup).
error_context_web(Context, [element(p, [], [AtomicContext])]):-
  term_to_atom(Context, AtomicContext).

error_context_web(
  PredicateName,
  ContextMessage,
  [
    element(p, [], [
      'The predicate that raised the error is: ',
      element(span, [class=term], [PredicateName]),
      '.']
    ),
    element(p, [], [
      'The additional description of the error is: ',
      element(span, [class=emphasis], [ContextMessage])]
    )
  ]).

%% error_formal_web(+Formal:compound, -Markup:list) is det.
% Returns markup for the given formal error term.

% Domain error.
error_formal_web(
  domain_error(Type, Term),
  [
    element(b, [], ['Domain error']),
    element(p, [], [
      'The term ',
      element(span, [class=term], [Term]),
      ' is of the proper type (i.e, ',
      element(span, [class=type], [Type]),
      ' but its value is outside of the supported values.']
    )
  ]
):-
  !.
% Existence error.
error_formal_web(
  existence_error(Type, Term),
  [
    element(b, [], ['Existence error']),
    element(p, [], [
      'Term ',
      element(span, [class=term], [Term]),
      ' is of the proper type (i.e., ',
      element(span, [class=type], [Type]),
      ' and correct domain, but there is no existing (external) resource \c
       that is represented by it.']
    )
  ]
):-
  !.
% Instantiation error.
error_formal_web(
  instantiation_error(_Term),
  [
    element(b, [], ['Instantiation error']),
    element(p, [], [
      'An argument is under-instantiated. I.e. it  is not acceptable as it \c
       is, but if some variables are  bound to appropriate values it would \c
       be acceptable.']
    )
  ]
):-
  !.
% Permission error.
error_formal_web(
  permission_error(Action, Type, Term),
  [
    element(b, [], ['Permission error']),
    element(p, [], [
      'It is not allowed to perform action ',
      element(span, [class=action], [Action]),
      ' on the object ',
      element(span, [class=term], [Term]),
      ' that is of type ',
      element(span, [class=type], [Type]),
      '.']
    )
  ]
):-
  !.
% Representation error.
error_formal_web(
  representation_error(Reason),
  [
    element(b, [], ['Representation error']),
    element(p, [], [
      'A limitation of the current Prolog implementation is breached: ',
      element(span, [class=emphasis], Reason),
      '.']
    )
  ]
):-
  !.
% Shell error.
error_formal_web(
  shell_error(Culprit),
  [
    element(b, [], ['Syntax error']),
    element(p, [], [
      'The shell encountered an error: ',
      element(span, [class=emphasis], [Culprit]),
      '.']
    )
  ]
):-
  !.
% Syntax error.
error_formal_web(
  syntax_error(Culprit),
  [
    element(b, [], ['Syntax error']),
    element(p, [], [
      'A text has invalid syntax: ',
      element(span, [class=syntax], [Culprit]),
      '.']
    )
  ]
):-
  !.
% Type error.
error_formal_web(
  type_error(Type, Term),
  [
    element(b, [], ['Type error.']),
    element(p, [], [
      'Term ',
      element(span, [class=term], [Term]),
      ' is not of type ',
      element(span, [class=type], [Type]),
      '.']
    )
  ]
):-
  !.
error_formal_web(Formal, [element(p, [], [FormalAtom])]):-
  term_to_atom(Formal, FormalAtom).

%% error_web(+Error:error, -Markup:list) is det.
% Returns the markup for the given error or exception.
% An error or exception consists of a formal description and a context
% description. Both are rendered into markup.
%
% @param Error An error or exception of the form =error(Formal, Context)=.
% @param Markup A list of XML elements.

error_web(error(Formal, Context), Markup):-
  error_formal_web(Formal, FormalMarkup),
  error_context_web(Context, ContextMarkup),
  append(
    [[element(h1, [], ['Error']), element(h2, [], ['Formal']) | FormalMarkup],
     [element(h2, [], ['Context']) | ContextMarkup]],
    Markup
  ).

%% error_web(+Format, +Arguments:list, -Markup:list) is det.
% This supports markup generation for simple 'error' statements.
% The use of error_web/2, with a proper error or exception compound term,
% if prefered.
%
% @see error_web/2

error_web(
  Format,
  Arguments,
  [element(h1, [], ['Error']), element(p, [], [ErrorMessage])]
):-
  format(atom(ErrorMessage), Format, Arguments).

predicate_to_atom(Module:Predicate/Arity, Atom):-
  !,
  format(atom(Atom), '~w:~w/~w', [Module, Predicate, Arity]).
predicate_to_atom(Predicate/Arity, Atom):-
  !,
  format(atom(Atom), '~w/~w', [Predicate, Arity]).
predicate_to_atom(Predicate, Predicate).

