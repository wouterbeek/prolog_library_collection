:- module(
  reflexive_closure,
  [
    reflexive_closure/2 % +Set:ordset(pair)
                        % -Closure:ordset(pair)
  ]
).

:- use_module(library(set/ordset_closure)).

:- chr_constraint(leq/2).

reflexivity  @ leq(X,X)            <=> true .
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true .

reflexive_closure(S1, S2):-
  ordset_closure(S1, S2, reflexive_closure).
