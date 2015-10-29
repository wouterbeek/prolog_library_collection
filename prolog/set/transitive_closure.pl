:- module(
  transitive_closure,
  [
    transitive_closure/2 % +Set:ordset(pair)
                         % -Closure:ordset(pair)
  ]
).

:- use_module(library(plunit)).
:- use_module(library(set/ordset_closure)).

:- chr_constraint(leq/2).

transitivity @ leq(X,Y), leq(Y,Z)  ==> leq(X,Z) .
idempotence  @ leq(X,Y) \ leq(X,Y) <=> true     .

transitive_closure(S1, S2):-
  ordset_closure(S1, S2, transitive_closure).

:- begin_tests('transitive_closure/2').

test(
  'transitive_closure(+,-) is det. TRUE',
  [forall(transitive_closure_test(GName,Closure))]
):-
  s_graph_test(GName, G),
  transitive_closure(G, Closure0),
  Closure == Closure0.

transitive_closure_test(path(2), [1-[1,2],2-[1,2]]).

:- end_tests('transitive_closure/2').
