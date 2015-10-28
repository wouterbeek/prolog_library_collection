:- module(
  symmetric_closure,
  [
    symmetric_closure/2 % +Set:ordset(pair)
                        % -Closure:ordset(pair)
  ]
).

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(chr_ext)).
:- use_module(library(dcg/dcg_pl_term)).

:- chr_constraint(leq/2).

:- debug(symmetric_closure).

%antisymmetry @ leq(X,Y), leq(Y,X)  <=> X = Y    .
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true     .
%reflexivity  @ leq(X,X)            <=> true     .
symmetry     @ leq(X,Y)            ==> leq(Y,X) .
%transitivity @ leq(X,Y), leq(Y,Z)  ==> leq(X,Z) .

symmetric_closure(S1a, S2a):-
  maplist(pair_term, S1a, S1b),
  chr_closure(S1b, S2b, symmetric, symmetric_closure, leq),
  maplist(pair_term, S2a, S2b).

pair_term(X-Y, leq(X,Y)).

leq(leq(X,Y)) -->
  pl_term(X),
  " ≤ ",
  pl_term(Y).
