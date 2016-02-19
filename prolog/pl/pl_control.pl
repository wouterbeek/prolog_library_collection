:- module(
  pl_control,
  [
    loop_until_true/1 % :Goal_0
  ]
).

/** <module> Prolog control structures

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(debug)).

:- meta_predicate
    loop_until_true(0),
    loop_until_true0(+, 0).





%! loop_until_true(:Goal_0) .

loop_until_true(Goal_0):-
  catch(Goal_0, E, loop_until_true0(E, Goal_0)).

loop_until_true0(E, _):-
  var(E), !.
loop_until_true0(E, Goal_0):-
  debug(loop_until_true, "~w", E),
  Goal_0.
