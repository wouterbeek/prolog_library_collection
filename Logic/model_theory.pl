:- module(
  model_theory,
  [
    add_domain/1, % +Object:atom
    add_relation/3, % +Arity:integer
                    % +Term:atom
                    % +Objects:list(atom)
    clear_model/0,
    domain/1, % ?Object:atom
    print_model/0,
    satisfy/1, % ?Sentence:compound
    satisfy/3 % ?Expression:compound
              % ?G:ord_set(assignment)
              % ?Polarity:oneof([neg,pos])
  ]
).

/** <module> MODEL THEORY

---+ Assignment

A compound term of the form =|g(-Var,+Val:atom)|=

@author Wouter Beek
@version 2013/05
*/

:- use_module(generics(db_ext)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).

%% domain(?Object:atom) is nondet.
:- dynamic(domain(_Object)).
%% f(?Arity:integer, ?Term:atom, ?Objects:list(atom)) is nondet.
:- dynamic(f(_Arity, _Term, _Objects)).



/*
%% beta_convert(+Expression, -Result)

beta_convert(Expression, Result):-
  beta_convert(Expression, Result, []).

beta_convert(Expression, Result, []):-
  var(Expression),
  Result = Expression.
beta_convert(Expression, Result, Stack):-
  nonvar(Expression),
  Expression = app(Functor, Argument),
  nonvar(Functor),
  beta_convert(Functor, Result, [Argument | Stack]).
beta_convert(Expression, Result, [X | Stack]):-
  nonvar(Expression),
  Expression = lambda(X, Formula),
  beta_convert(Formula, Result, Stack).
beta_convert(Expression, Result, []):-
  nonvar(Expression),
  not((Expression = app(X, _), nonvar(X))),
  Expression =.. [Functor | SubExpressions],
  maplist(beta_convert, SubExpressions, ResultSubExpressions),
  Result =.. [Functor | ResultSubExpressions].

satisfy(lambda(X, Type, Formula), G, pos, Mode):-
  satisfy(exists(X, Type, Formula), G, neg, Mode),
  assert(answer([])).

satisfy(lambda(X, Type, Formula), G, pos, Mode):-
  bagof(
    [g(X, V) | G],
    (
      domain(Type, V),
      ( Mode == identifier ->
        f(0, V2, V)
      ;
        V2 = V
      ),
      satisfy(Formula, [g(X, V2) | G], pos, Mode)
    ),
    Answers
  ),
  forall(
    member(Answer, Answers),
    assert(answer(Answer))
  ).
*/

%% add_domain(+Object:atom) is det.

add_domain(Object):-
  assert_novel(domain(Object)).

add_relation(Arity, Term, Extension):-
  assert_novel(f(Arity, Term, Extension)).

clear_model:-
  retractall(domain(_)),
  retractall(f(_, _, _)).

load_model(test1):-
  maplist(add_domain, [d1,d2,d3,d4,d5]),
  maplist(
    add_relation(0),
    ['Jules','Vincent','Pumpkin','Honey Bunny','Yolanda'],
    [d1,d2,d3,d4,d5]
  ),
  add_relation(1, customer, [[d1],[d2]]),
  add_relation(1, robber, [[d3],[d4]]),
  add_relation(2, love, [[d3,d4]]).
load_model(test2):-
  maplist(add_domain, [d1,d2,d3,d4,d5,d6]),
  maplist(
    add_relation(0),
    ['Jules','Vincent','Pumpkin','Honey Bunny','Yolanda'],
    [d1,d2,d3,d4,d4]
  ),
  add_relation(1, customer, [[d1],[d2],[d5],[d6]]),
  add_relation(1, robber, [[d3],[d4]]),
  add_relation(2, love, []).
load_model(test3):-
  maplist(add_domain, [d1,d2,d3,d4,d5,d6,d7,d8]),
  maplist(add_relation(0), [mia,jody,'Jules','Vincent'], [d1,d2,d3,d4]),
  add_relation(1, woman, [[d1],[d2]]),
  add_relation(1, man, [[d3],[d4]]),
  add_relation(1, joke, [[d5],[d6]]),
  add_relation(1, episode, [[d7],[d8]]),
  add_relation(2, in, [[d5,d7],[d5,d8]]),
  add_relation(2, tell, [[d1,d5],[d2,d6]]).

print_domain:-
  format(user_output, 'DOMAIN:\n', []),
  forall(
    f(0, Constant, Object),
    format(user_output, '\t~w:\t~w\n', [Constant, Object])
  ).

print_model:-
  print_domain,
  print_relations.

print_relations:-
  format(user_output, 'PREDICATES:\n', []),
  forall(
    (
      f(Arity, Term, Tuples),
      Arity \== 0
    ),
    (
      format(user_output, '\t~w/~w:\t', [Term, Arity]),
      findall(
        TupleAtom,
        (
          member(Tuple, Tuples),
          atomic_list_concat(Tuple, ',', ObjectsAtom),
          format(atom(TupleAtom), '<~w>', [ObjectsAtom])
        ),
        TupleAtoms
      ),
      atomic_list_concat(TupleAtoms, ', ', TuplesAtom),
      format(user_output, '{~w}\n', [TuplesAtom])
    )
  ).

satisfy(Sentence):-
  satisfy(Sentence, [], pos).

%% satisfy(+Expression, +G, +Polarity)
% Checks whether the given formula is true in the given model, under the
% given polarity.
% The polarity is usefull, since it allows us to easily cope with negative
% literals.

% Negation
satisfy(not(A), G, pos):-
  satisfy(A, G, neg).
satisfy(not(A), G, neg):-
  satisfy(A, G, pos).
% Conjunction
satisfy(and(A, B), G, pos):-
  satisfy(A, G, pos),
  satisfy(B, G, pos).
satisfy(and(A, B), G, neg):-
  satisfy(A, G, neg);
  satisfy(B, G, neg).
% Disjunction
satisfy(or(A, B), G, pos):-
  satisfy(A, G, pos);
  satisfy(B, G, pos).
satisfy(or(A, B), G, neg):-
  satisfy(A, G, neg),
  satisfy(B, G, neg).
% Implication
satisfy(imp(A, B), G, Polarity):-
  satisfy(or(not(A), B), G, Polarity).
% Biimplication
satisfy(bi(A, B), G, Polarity):-
  satisfy(and(imp(A, B), imp(B, A)), G, Polarity).
% Existential quantification
satisfy(exists(X, A), G, pos):-
  domain(C),
  ord_add_element(G, g(X,C), NewG),
  satisfy(A, NewG, pos).
satisfy(exists(X, A), G, neg):-
  forall(
    domain(C),
    (
      ord_add_element(G, g(X,C), NewG),
      satisfy(A, NewG, neg)
    )
  ).
% Universal quantification
satisfy(forall(X, A), G, Polarity):-
  satisfy(not(exists(X, not(A))), G, Polarity).

% Identity relation
satisfy(identity(A1, A2), G, pos):-
  i(A1, G, B),
  i(A2, G, B).
satisfy(identity(A1, A2), G, neg):-
  i(A1, G, B1),
  i(A2, G, B2),
  B1 \== B2.

% Atomic sentences
satisfy(A, G, pos):-
  A =.. [Predicate | Arguments],
  maplist(i(G), Arguments, Objects),
  length(Arguments, Arity),
  f(Arity, Predicate, Extension),
  member(Objects, Extension).
satisfy(A, G, neg):-
  A =.. [Predicate | Arguments],
  maplist(i(G), Arguments, Objects),
  length(Arguments, Arity),
  \+ (f(Arity, Predicate, Extension),
    member(Objects, Extension)).

%% i(+G:ord_set(assignment), ?X:atom, -V:atom) is det.
% The interpretation function.
%
% @param G The assignment function.
% @param X The constant or variable that is interpreted.
% @param V The interpretation of X.

% X is a variable. The assignment function is used to establish the
% interpretation of X.
i(G, X, V):-
  var(X),
  member(g(Y, V), G),
  Y == X.
% X is a constant. We look up the interpretation of X in the domain.
i(_G, X, V):-
  atom(X),
  f(0, X, V).
