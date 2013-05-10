:- module(
  thread_ext,
  [
    thread_alias/1, % ?ThreadAlias:atom
    thread_end/1, % +ThreadAlias:atom
    thread_overview/0,
    thread_overview_web/1, % -Markup:dom
    thread_recover/0,
    thread_recover/1, % +ThreadAlias:atom
    thread_reset/1, % +ThreadAlias:atom
    thread_restart/1, % +ThreadAlias:atom
    thread_start/4, % +Model:atom
                    % :Goal
                    % +TaskList:list
                    % ?ThreadAlias:atom
    thread_success/1 % +ThreadAlias:atom
  ]
).

/** <module> Thread extensions

Allows one to monitor running threads that register.

@author Wouter Beek
@version 2013/03
*/

:- use_module(generics(atom_ext)).
:- use_module(generics(meta_ext)).

:- dynamic(end_flag(_ThreadAlias, _NumberOfTasks)).
:- dynamic(workload(_ThreadAlias, _Module, _Goal, _TaskList)).



thread_alias(ThreadAlias):-
  nonvar(ThreadAlias),
  !,
  atom_concat('t', _, ThreadAlias).
thread_alias(ThreadAlias):-
  flag(thread_alias, ID, ID + 1),
  format_integer(ID, 2, ID1),
  format(atom(ThreadAlias), 't~w', [ID1]).

thread_end(ThreadAlias):-
  thread_property(ThreadId, alias(ThreadAlias)),
  thread_join(ThreadId, _Status),
  retract(end_flag(ThreadAlias, _)),
  retract(workload(ThreadAlias, _Module, _Goal, _TaskList)),
  flag(ThreadAlias, _OldId, 0).

thread_overview:-
  thread_overview(Atoms),
  forall(
    member(Atom, Atoms),
    format(user, '~w\n', [Atom])
  ).

thread_overview(Atoms):-
  setoff(
    ThreadAlias/Current/End,
    (
      end_flag(ThreadAlias, End),
      flag(ThreadAlias, Current, Current)
    ),
    Triples
  ),
  findall(
    Atom,
    (
      member(ThreadAlias/Current/End, Triples),
      progress_bar(Current, End, ProgressBar),
      thread_status(ThreadAlias, Status),
      format(atom(Atom), '~w ~w {~w}', [ThreadAlias, ProgressBar, Status])
    ),
    Atoms
  ).

thread_overview_web(Markup):-
  thread_overview(Atoms),
  findall(
    element(pre, [], [Atom]),
    member(Atom, Atoms),
    Markup
  ).

thread_recover:-
  forall(
    end_flag(ThreadAlias, _),
    thread_recover(ThreadAlias)
  ).

thread_recover(ThreadAlias):-
  thread_status(ThreadAlias, Status),
  (
    Status == running
  ->
    true
  ;
    Status = exception(Exception)
  ->
    format(user, 'Thread ~w exception ~w.\n', [ThreadAlias, Exception]),
    thread_reset(ThreadAlias)
  ;
    thread_end(ThreadAlias),
    format(user, 'Thread ~w was finished.\n', [ThreadAlias])
  ).

thread_reset(ThreadAlias):-
  workload(ThreadAlias, Module, Goal, TaskList),
  flag(ThreadAlias, ID, ID),
  (ID == 0 -> ProcessedLength = 0 ; ProcessedLength is ID - 1),
  thread_end(ThreadAlias),
  length(Processed, ProcessedLength),
  append(Processed, Unprocessed, TaskList),
  once(thread_start(Module, Goal, Unprocessed, ThreadAlias)).

thread_restart(ThreadAlias):-
  workload(ThreadAlias, Module, Goal, TaskList),
  once(thread_start(Module, Goal, TaskList, _ThreadId)).

thread_start(Module, Goal, TaskList, ThreadAlias):-
  Goal =.. [Predicate | Arguments],
  append(Arguments, [TaskList], NewArguments),
  NewGoal =.. [Predicate | NewArguments],
  thread_alias(ThreadAlias),
  thread_create(Module:NewGoal, _ThreadId, [alias(ThreadAlias)]),
  length(TaskList, NumberOfTasks),
  assert(end_flag(ThreadAlias, NumberOfTasks)),
  assert(workload(ThreadAlias, Module, Goal, TaskList)).

thread_status(ThreadAlias, Status):-
  thread_property(ThreadId, alias(ThreadAlias)),
  thread_property(ThreadId, status(Status)).

thread_success(ThreadAlias):-
  flag(ThreadAlias, ID, ID + 1),
  if_then(
    end_flag(ThreadAlias, ID),
    thread_end(ThreadAlias)
  ).
