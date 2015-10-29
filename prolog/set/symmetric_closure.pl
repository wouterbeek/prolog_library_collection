:- module(
  symmetric_closure,
  [
    symmetric_closure/2 % +Set:ordset(pair)
                        % -Closure:ordset(pair)
  ]
).

:- use_module(library(set/ordset_closure)).

:- chr_constraint(leq/2).

symmetry     @ leq(X,Y)            ==> leq(Y,X) .
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true     .

symmetric_closure(S1, S2):-
  ordset_closure(S1, S2, symmetric_closure).
