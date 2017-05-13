:- module(
  closure,
  [
    closure_set/3, % :Goal_2, +From, -Closure
    closure0_set/3 % :Goal_2, +From, -Closure
  ]
).

/** <module> Closures

Closures come in handy in many Prolog programs!

I have merely collected these predicates, they are created by
others. See the predicate documentations for the original authors and
pointers to related content.

@version 2015/10, 2016/05
*/

:- use_module(library(aggregate)).

:- meta_predicate
    closure_set(2, +, -),
    closure0_set(2, +, -).





%! closure_set(:Goal_2, +Element, -Closure) is det.

closure_set(Goal_2, X1, S):-
  aggregate_all(set(X2), closure(Goal_2, X1, X2), S).



%! closure0_set(:Goal_2, +Element, -Closure) is det.

closure0_set(Goal_2, X1, S):-
  aggregate_all(set(X2), closure0(Goal_2, X1, X2), S).
