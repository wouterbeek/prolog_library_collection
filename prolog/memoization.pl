:- module(
  memoization,
  [
    memo/1, % :Goal_0
    reset_memos/0
  ]
).

/** <module> Memoization

@author Wouter Beek
@version 2015/08, 2016/04
*/

:- meta_predicate(memo(0)).

:- dynamic(memo0/1).





%! memo(:Goal_0) is det.
% Memo goals that take relatively long to compute and that
% are likely to be recomputed in the future.
% This is achieved by storing the result along with the call,
% i.e. the fully instantiated goal.
% There are no restrictions on the determinism of the goal.

memo(Goal_0):-
  \+ memo0(Goal_0), !,
  call(Goal_0),
  assertz(memo0(Goal_0)).
memo(Goal_0):-
  memo0(Goal_0).



%! reset_memos is det.

reset_memos:-
  retractall(memo0(_)).
