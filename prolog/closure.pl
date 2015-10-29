:- module(
  closure,
  [
    closure/3, % :Goal_2
               % +From
               % -To
    closure0/3 % :Goal_2
               % +From
               % -To
  ]
).

/** <module> Closures

Closures come in handy in many Prolog programs!

@author Wouter Beek
        I have merely collected these predicates, they are
        created by others. See the predicate documentations
        for the original authors and pointers to related content.
@version 2014/11, 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dif)).

:- meta_predicate(closure(2,+,-)).
:- meta_predicate(closure0(2,+,-)).
:- meta_predicate(closure0(2,+,-,+)).





%! closure(:Goal_2, +X1, -X2) is nondet.
% Calculates the transitive closure of binary predicate `Goal_2`.
%
% @author Ulrich Neumerkel
% @see [StackOverflow](http://stackoverflow.com/questions/26964782/determining-if-graph-is-connected-in-prolog/26965843?noredirect=1#comment42472120_26965843)
%      for a discussion about the predicate names closure/3 and closure0/3
% @see [StackOverflow](http://stackoverflow.com/questions/15473065/hilog-terms-in-xsb-prolog/15483764#15483764)
%      for the definition of closure/3 in terms of closure0/3.

closure(Goal_2, X0, X):-
  call(Goal_2, X0, X1),
  closure0(Goal_2, X1, X, [X0,X1]).



%! closure0(:Goal_2, +X1, -X2) is multi.
% Calculates the transitive-reflexive closure of binary predicate `Goal_2`.
%
% @author Ulrich Neumerkel
% @see [StackOverflow](http://stackoverflow.com/questions/26946133/definition-of-reflexive-transitive-closure)
%      for the definition of closure0/3.
% @see [StackOverflow](http://stackoverflow.com/questions/26964782/determining-if-graph-is-connected-in-prolog/26965843?noredirect=1#comment42472120_26965843)
%      for a discussion about the predicate names closure/3 and closure0/3

closure0(Goal_2, X0, X):-
  closure0(Goal_2, X0, X, [X0]).

closure0(_, X, X, _).
closure0(Goal_2, X1, X, Hist):-
  call(Goal_2, X1, X2),
  maplist(dif(X2), Hist),
  closure0(Goal_2, X2, X, [X1|Hist]).
