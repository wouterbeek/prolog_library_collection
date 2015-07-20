:- module(
  pl_mode,
  [
    call_complete/3, % :Goal
                     % +Input
                     % -History:list
    call_count/2, % :Goal
                  % -Number:number
    call_det/1, % :Goal
    call_det/2, % :Goal
                % +Argument1:pair
    call_det/3, call_det/4, call_det/5, call_det/6, call_det/7, call_det/8,
    call_ground_as_semidet/1, % :Goal
    call_mode/2, % +Mode:oneof([det,multi,nondet,semidet])
                 % :Goal
    call_multi/2, % :Goal
                  % +Count:integer
    call_multi/4, % :Goal
                  % +Count:integer
                  % +Input:term
                  % -Output:term
    call_multi/5, % :Goal
                  % +Count:integer
                  % +Input:term
                  % -Output:term
                  % -History:list(term)
    call_nth/2, % :Goal
                % +N:nonneg
    call_semidet/1, % :Goal
    enforce_mode/3 % :Goal
                   % +Arguments:list
                   % +Declaration:list(pair(list(oneof(['+','-','?'])),oneof([det,multi,nondet,semidet])))
  ]
).

/** <module> Prolog modes

Automated checks for Prolog mode enforcement.

@author Wouter Beek
@version 2012/07-2012/08, 2013/01, 2013/03-2013/04, 2013/09-2013/10, 2013/12,
         2014/07, 2014/10
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).

:- use_module(plc(generics/error_ext)).

:- meta_predicate(call_complete(2,+,-)).
:- meta_predicate(call_count(0,-)).
:- meta_predicate(call_det(0)).
:- meta_predicate(call_det(1,+)).
:- meta_predicate(call_det(2,+,+)).
:- meta_predicate(call_det(3,+,+,+)).
:- meta_predicate(call_det(4,+,+,+,+)).
:- meta_predicate(call_det(5,+,+,+,+,+)).
:- meta_predicate(call_det(6,+,+,+,+,+,+)).
:- meta_predicate(call_det(7,+,+,+,+,+,+,+)).
:- meta_predicate(call_ground_as_semidet(0)).
:- meta_predicate(call_mode(+,0)).
:- meta_predicate(call_multi(0,+)).
:- meta_predicate(call_multi(2,+,+,-)).
:- meta_predicate(call_multi(2,+,+,-,-)).
:- meta_predicate(call_nth(0,-)).
:- meta_predicate(call_semidet(0)).
:- meta_predicate(enforce_mode(0,+,+)).



args_instantiation([], []).
args_instantiation([H|T1], ['+'|T2]):- !,
  nonvar(H),
  args_instantiation(T1, T2).
args_instantiation([H|T1], ['-'|T2]):- !,
  var(H),
  args_instantiation(T1, T2).
args_instantiation([_|T1], [_|T2]):-
  args_instantiation(T1, T2).


%! call_complete(:Goal, +Input, -Results:list) is det.
% Runs the given goal on the given input until it wears out.
% The goal is enforced to be deteministic or semi-deterministic
% (the extra choicepoint is automatically dropped).
%
% @tbd Check whether this can be unified with multi/[4,5].

call_complete(Goal, Input, [Input|History]):-
  once(call(Goal, Input, Output)),
  Input \== Output, !,
  call_complete(Goal, Output, History).
call_complete(_, Input, [Input]).


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


call_det(Goal):-
  catch(Goal, _, mode_error(det, Goal)).

call_det(Goal, Type1-Arg1):-
  is_of_type(Type1, Arg1), !,
  call(Goal, Arg1), !.
call_det(Goal, _-Arg1):-
  call(Goal, Arg1).

call_det(Goal, Type1-Arg1, Type2-Arg2):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2), !,
  call(Goal, Arg1, Arg2), !.
call_det(Goal, _-Arg1, _-Arg2):-
  call(Goal, Arg1, Arg2).

call_det(Goal, Type1-Arg1, Type2-Arg2, Type3-Arg3):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3), !,
  call(Goal, Arg1, Arg2, Arg3), !.
call_det(Goal, _-Arg1, _-Arg2, _-Arg3):-
  call(Goal, Arg1, Arg2, Arg3).

call_det(Goal, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4), !,
  call(Goal, Arg1, Arg2, Arg3, Arg4), !.
call_det(Goal, _-Arg1, _-Arg2, _-Arg3, _-Arg4):-
  call(Goal, Arg1, Arg2, Arg3, Arg4).

call_det(Goal, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4, Type5-Arg5):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4),
  is_of_type(Type5, Arg5), !,
  call(Goal, Arg1, Arg2, Arg3, Arg4, Arg5), !.
call_det(Goal, _-Arg1, _-Arg2, _-Arg3, _-Arg4, _-Arg5):-
  call(Goal, Arg1, Arg2, Arg3, Arg4, Arg5).

call_det(Goal, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4, Type5-Arg5, Type6-Arg6):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4),
  is_of_type(Type5, Arg5),
  is_of_type(Type6, Arg6), !,
  call(Goal, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6), !.
call_det(Goal, _-Arg1, _-Arg2, _-Arg3, _-Arg4, _-Arg5, _-Arg6):-
  call(Goal, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6).

call_det(Goal, Type1-Arg1, Type2-Arg2, Type3-Arg3, Type4-Arg4, Type5-Arg5, Type6-Arg6, Type7-Arg7):-
  is_of_type(Type1, Arg1),
  is_of_type(Type2, Arg2),
  is_of_type(Type3, Arg3),
  is_of_type(Type4, Arg4),
  is_of_type(Type5, Arg5),
  is_of_type(Type6, Arg6),
  is_of_type(Type7, Arg7), !,
  call(Goal, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7), !.
call_det(Goal, _-Arg1, _-Arg2, _-Arg3, _-Arg4, _-Arg5, _-Arg6, _-Arg7):-
  call(Goal, Arg1, Arg2, Arg3, Arg4, Arg5, Arg6, Arg7).



%! call_ground_as_semidet(:Goal)

call_ground_as_semidet(Goal):-
  strip_module(Goal, _, Plain),
  Plain =.. [_|Args],
  maplist(ground, Args), !,
  Goal, !.
call_ground_as_semidet(Goal):-
  Goal.


call_mode(det, Goal):- !,
  call_det(Goal).
call_mode(semidet, Goal):- !,
  call_semidet(Goal).
call_mode(_Mode, Goal):-
  call(Goal).


%! call_nth(:Goal, +N:nonneg) is semidet.
% Calls the given goal the given number of times.
%
% This does not exclude the case in which the goal
%  could have been executed more than `N` times.
%
% @arg Goal A nondeterministic goal.
% @arg N A nonnegative integer.
%
% @author Ulrich Neumerkel

call_nth(Goal, C):-
  State = count(0),
  Goal,
  arg(1, State, C1),
  C2 is C1 + 1,
  nb_setarg(1, State, C2),
  C = C2.


%! call_semidet(:Goal) is det.
% Executes the given semi-deterministic goal exactly once,
%  i.e., regardless of any open choice points.
% If the goal is not semideterministic, an error is thrown.
%
% @author Ulrich Neumerkel
% @error error(mode_error(semidet, Goal),
%        context(call_semidet/1, 'Message left empty.'))

call_semidet(Goal):-
  (   call_nth(Goal, 2)
  ->  mode_error(semidet, Goal)
  ;   once(Goal)
  ).


enforce_mode(Goal, Args, Declaration):-
  member(Instantiation-Mode, Declaration),
  args_instantiation(Args, Instantiation), !,
  call_mode(Mode, Goal).
enforce_mode(Goal, _, _):-
  call_mode(_UnknownMode, Goal).


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

