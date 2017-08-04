:- module(
  ordset_closure,
  [
    ordset_closure/3 % +Set:ordset(pair)
                     % -Closure:ordset(pair)
                     % +Module:atom
  ]
).
:- reexport(library(chr_ext)).

/** <module> Ordered set closure

Every module `Mod` is a debug flag `set_closure(Mod)` as well.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_pl)).





%! ordset_closure(+Set:ordset(pair), -Closure:ordset(pair), +Module) is det.

ordset_closure(S1a, S2a, Mod):-
  maplist(pair_term, S1a, S1b),
  chr_closure(S1b, S2b, Mod, set_closure(Mod), leq),
  maplist(pair_term, S2a, S2b).


pair_term(X-Y, leq(X,Y)).


leq(leq(X,Y)) --> pl_term(X), " ≤ ", pl_term(Y).
