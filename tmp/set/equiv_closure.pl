:- module(
  equiv_closure,
  [
    equiv_closure/2 % +Set:ordset(pair)
                    % -Closure:ordset(pair)
  ]
).

/** <module> Equivalence closure

@author Wouter Beek
@version 2015/12
*/

:- use_module(library(chr)).
:- use_module(library(set/ordset_closure)).

:- chr_constraint(leq/2).

idempotence  @ leq(X,Y) \ leq(X,Y) <=>           true               .
reflexivity  @ leq(X,Y)            ==> X \== Y | leq(X,X), leq(Y,Y) .
transitivity @ leq(X,Y), leq(Y,Z)  ==>           leq(X,Z)           .
symmetry     @ leq(X,Y)            ==>           leq(Y,X)           .

equiv_closure(S1, S2):-
  ordset_closure(S1, S2, equiv_closure).
