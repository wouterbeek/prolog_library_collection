:- module(
  pl_log,
  [
    run_collect_messages/1, % :Goal
    run_collect_messages/3 % :Goal
                           % -Status:or([oneof([false,true]),compound])
                           % -Messages:list(compound)
  ]
).

/** <module> Prolog logging

Logging the performance and results of Prolog predicates.

@author Wouter Beek
@version 2014/04, 2014/06, 2014/08-2014/09, 2014/12, 2015/03
*/

:- use_module(library(check_installation)). % Private predicates.
:- use_module(library(debug)).

:- meta_predicate(run_collect_messages(0)).
:- meta_predicate(run_collect_messages(0,-,-)).





%! run_collect_messages(:Goal) .
% Write an abbreviated version of status and messages to the console.

run_collect_messages(Goal):-
  run_collect_messages(Goal, Status, Warnings),
  process_warnings(Warnings),
  process_status(Status).



%! run_collect_messages(
%!   :Goal,
%!   -Status:or([oneof([false,true]),compound]),
%!   -Messages:list(compound)
%! ) is det.
% Return status and messages to the calling context for further processing.

run_collect_messages(Goal, Status, Messages):-
  check_installation:run_collect_messages(Goal, Status, Messages).





% HELPERS %

process_status(true):- !.
process_status(fail):- !,
  fail.
process_status(Exception):-
  print_message(warning, Exception).



process_warnings(Warnings):-
  debugging(pl_log), !,
  forall(
    member(Warning, Warnings),
    debug(pl_log, '[WARNING] ~a', [Warning])
  ).
process_warnings(Warnings):-
  length(Warnings, NumberOfWarnings),
  (   NumberOfWarnings =:= 0
  ->  true
  ;   print_message(warnings, number_of_warnings(NumberOfWarnings))
  ).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(number_of_warnings(NumberOfWarnings)) -->
  ['~D warnings'-[NumberOfWarnings]].
