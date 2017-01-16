:- module(
  call_ext,
  [
    call_catcher_cleanup/3, % :Goal_0, +Catcher, :Cleanup_0
    call_det/2,             % :Goal_0, +IsDet
    call_n_sol/3,           % +N, :Select_1, :Goal_1
    call_n_times/2,         % +N, :Goal_0
    call_or_exception/1,    % :Goal_0
    call_or_fail/1,         % :Goal_0
    call_timeout/2,         % +Time, :Goal_0
    catch_msg/1,            % :Goal_0
    catch_msg/3,            % +Def1, :Goal_1, -Arg1
    concurrent_n_sols/3,    % +N, :Select_1, :Goal_1
    forall/1,               % :Goal_0
    retry0/1,               % :Goal_0
    var_goal/1              % @Term
  ]
).

/** <module> Call extensions

@author Wouter Beek
@version 2016/04-2017/01
*/

:- use_module(library(debug)).
:- use_module(library(lists)).
:- use_module(library(thread)).
:- use_module(library(time)).

:- meta_predicate
    call_catcher_cleanup(0, +, 0),
    call_det(0, +),
    call_n_sol(+, 1, 1),
    call_n_times(+, 0),
    call_or_exception(0),
    call_or_fail(0),
    call_timeout(+, 0),
    catch_msg(0),
    catch_msg(+, 1, -),
    concurrent_n_sols(+, 1, 1),
    forall(0),
    retry0(0).





%! call_catcher_cleanup(:Goal_0, +Catcher, :Cleanup_0) .

call_catcher_cleanup(Goal_0, Catcher, Cleanup_0) :-
  setup_call_catcher_cleanup(true, Goal_0, Catcher, Cleanup_0).



%! call_det(:Goal_0, +IsDet) is det.

call_det(Goal_0, true) :- !,
  once(Goal_0).
call_det(Goal_0, false) :-
  call(Goal_0).



%! call_n_sol(+N, :Select_1, :Goal_1) is det.

call_n_sol(N, Select_1, Goal_1) :-
  findnsols(N, X, call(Select_1, X), Xs),
  last(Xs, X),
  call(Goal_1, X).



%! call_n_times(+N, :Goal_0) is det.

call_n_times(N, Goal_0) :-
  forall(between(1, N, _), Goal_0).



%! call_or_exception(:Goal_0) is semidet.

call_or_exception(Goal_0) :-
  catch(Goal_0, E, true),
  (   var(E)
  ->  true
  ;   print_message(warning, E),
      fail
  ).



%! call_or_fail(:Goal_0) is semidet.

call_or_fail(Goal_0) :-
  catch(Goal_0, _, fail).



%! call_timeout(+Time, :Goal_0) is det.
%
% Time is a float, integer, or `inf`.

call_timeout(inf, Goal_0) :- !,
  call(Goal_0).
call_timeout(Time, Goal_0) :-
  call_with_time_limit(Time, Goal_0).



%! catch_msg(:Goal_0) is det.
%! catch_msg(+Def1, :Goal_1, -Arg1) is det.
%
% Catch all exceptions and failures in Goal_0/Goal_1 and print a
% message when this happens.

catch_msg(Goal_0) :-
  (catch(Goal_0, E, print_catch(E)) -> true ; print_catch("failed")).


catch_msg(Def1, Mod:Goal_1, Arg1) :-
  Goal_1 =.. [Pred|Args1],
  append(Args1, [Arg1], Args2),
  Goal_0 =.. [Pred|Args2],
  (   catch(Mod:Goal_0, E, print_catch(Def1, E, Arg1))
  ->  true
  ;   print_catch(Def1, "failed", Arg1)
  ).

print_catch(E) :-
  print_message(warning, ckan_error(E)).

print_catch(Arg1, E, Arg1) :-
  print_catch(E).



%! concurrent_n_sols(+N, :Select_1, :Goal_1) is det.

concurrent_n_sols(N, Select_1, Mod:Goal_1) :-
  findnsols(
    N,
    Mod:Goal_0,
    (
      call(Select_1, X),
      Goal_0 =.. [Goal_1,X]
    ),
    Goals
  ),
  concurrent(N, Goals, []).



%! forall(:Goal_0) is det.

forall(Goal_0) :-
  forall(Goal_0, true).



%! retry0(:Goal_0) is det.

retry0(Goal_0) :-
  catch(Goal_0, Exception, true),
  (var(Exception) -> true ; debug(true, "~w", [Exception]), retry0(Goal_0)).



%! var_goal(@Term) is semidet.
%
% Succeeds for a variable or a module prefixes variable.

var_goal(X) :- var(X), !.
var_goal(_:X) :- var(X).
