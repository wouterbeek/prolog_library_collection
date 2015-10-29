:- module(
  transitive_closure,
  [
    transitive_closure/2 % +Set:ordset(pair)
                         % -Closure:ordset(pair)
  ]
).

:- use_module(library(set/ordset_closure)).

:- chr_constraint(leq/2).

transitivity @ leq(X,Y), leq(Y,Z)  ==> leq(X,Z) .
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true     .

transitive_closure(S1, S2):-
  ordset_closure(S1, S2, transitive_closure).
