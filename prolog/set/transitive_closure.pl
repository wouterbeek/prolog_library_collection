:- module(
  transitive_closure,
  [
    transitive_closure/2 % +Set:ordset(pair)
                         % -Closure:ordset(pair)
  ]
).

:- use_module(library(graph/s/s_graph)).
:- use_module(library(plunit)).
:- use_module(library(set/ordset_closure)).

:- chr_constraint(leq/2).

idempotence  @ leq(X,Y) \ leq(X,Y) <=> true     .
transitivity @ leq(X,Y), leq(Y,Z)  ==> leq(X,Z) .

transitive_closure(S1, S2):-
  ordset_closure(S1, S2, transitive_closure).

:- begin_tests('transitive_closure/2').

test(
  'transitive_closure(+,-) is det. TRUE',
  [forall(transitive_closure_test(GName,Closure))]
):-
  s_test_graph(GName, G),
  s_graph_components(G, Vs, Es1),
  transitive_closure(Es1, Es2),
  s_graph_components(Closure, Vs, Es2).

transitive_closure_test(path(2), [1-[1,2],2-[1,2]]).

:- end_tests('transitive_closure/2').
