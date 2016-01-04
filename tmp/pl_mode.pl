:- module(
  pl_mode,
  [
    call_count/2, % :Goal_0
                  % -Number:nonneg
    call_ground_as_semidet/1, % :Goal_0
    call_multi/2, % :Goal_0
                  % +Count:nonneg
    call_multi/4, % :Goal_0
                  % +Count:nonneg
                  % +Input:term
                  % -Output:term
    call_multi/5 % :Goal_0
                 % +Count:nonneg
                 % +Input:term
                 % -Output:term
                 % -History:list(term)
  ]
).

/** <module> Prolog modes

Automated checks for Prolog mode enforcement.

@author Wouter Beek
@version 2015/10
*/

:- use_module(library(aggregate)).

:- use_module(plc(generics/error_ext)).

:- meta_predicate(call_count(0,-)).
:- meta_predicate(call_ground_as_semidet(0)).
:- meta_predicate(call_multi(0,+)).
:- meta_predicate(call_multi(2,+,+,-)).
:- meta_predicate(call_multi(2,+,+,-,-)).



%! call_count(:Goal, -Count:integer) is det.
% Returns the number of calls that can be made of the given goal.
%
% @arg Goal A goal.
% @arg Count An integer.

call_count(Goal1, Count):-
  strip_module(Goal1, _Module, Goal2),
  Goal2 =.. [_Pred|Args],
  aggregate_all(
    set(Args),
    Goal1,
    Argss
  ),
  length(Argss, Count).





%! call_ground_as_semidet(:Goal)

call_ground_as_semidet(Goal):-
  strip_module(Goal, _, Plain),
  Plain =.. [_|Args],
  maplist(ground, Args), !,
  Goal, !.
call_ground_as_semidet(Goal):-
  Goal.


%! call_multi(:Goal, +Count:nonneg) is det.
% Performs the given nondet goal the given number of times.

call_multi(_Goal, 0):- !.
call_multi(Goal, Count):-
  call(Goal),
  NewCount is Count - 1,
  call_multi(Goal, NewCount).

%! call_multi(:Goal, +Count:integer, +Input:term, -Output:term) is det.
% Applies a predicate multiple times on the given input and its
% subsequent outputs, i.e. repeated function application.
%
% @arg Goal
% @arg Count The integer counter, indicating the number of times the
%        predicate is applied repeaterly.
% @arg Input A term.
% @arg Output A term.

call_multi(Goal, Count, Input, Output):-
  call_multi(Goal, Count, Input, Output, _History).

call_multi(_Goal, 0, Output, Output, [Output]):- !.
call_multi(Goal, Count, Input, Output, [Intermediate | History]):-
  call(Goal, Input, Intermediate),
  NewCount is Count - 1,
  call_multi(Goal, NewCount, Intermediate, Output, History).
