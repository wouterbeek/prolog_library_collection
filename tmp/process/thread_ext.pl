:- module(
  thread_ext,
  [
    forall_thread/4, % :Antecedent
                     % :Consequent
                     % +DebugTopic:atom
                     % +DebugMessage:atom
    print_thread/1, % +Alias:atom
    print_threads/0,
    thread_create/1, % :Goal
% RUN ON SUBLISTS INFRASTRUCTURE
    intermittent_thread/5, % :Goal
                           % :EndGoal
                           % +Interval:positive_integer
                           % -Id
                           % +Options:list(nvpair)
    run_on_sublists/3, % +List:list
                       % :Goal
                       % +NumberOfThreads:positive_integer
    thread_alias/1, % ?ThreadAlias:atom
    thread_end/1, % +ThreadAlias:atom
    thread_overview/0,
    thread_overview_web/1, % -Markup:dom
    thread_prefix/2, % +Prefix:atom
                     % -Thread:atom
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
@version 2013/03, 2013/09, 2014/03-2014/04, 2014/12, 2015/02
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(debug)).
:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(generics/atom_ext)).
:- use_module(plc(generics/list_ext)).
:- use_module(plc(process/progress)).
:- use_module(plc(prolog/pl_control)).

:- meta_predicate(forall_thread(0,0,+,+)).
:- meta_predicate(intermittent_goal(0,0,+)).
:- meta_predicate(intermittent_thread(0,0,+,-,+)).
:- meta_predicate(run_on_sublists(+,1,+)).
:- meta_predicate(thread_create(0)).

:- dynamic(end_flag/2).
:- dynamic(workload/4).



%! forall_thread(
%!   :Antecedent,
%!   :Consequent,
%!   +DebugTopic:atom,
%!   +DebugMessage:atom
%! ) is det.
% Runs for instatiation of `Antecedent` a threaded instatiation `Consequent`.
% The threads are joined afterwards.
% The status is send to the debug stream if `Topic` is switched on.

forall_thread(Antecedent, Consequent, Topic, Msg):-
  findall(
    ThreadId,
    (
      call(Antecedent),
      thread_create(
        (
          call(Consequent),
          thread_at_exit(forall_thread_end(Topic, Msg))
        ),
        ThreadId,
        [detached(false)]
      )
    ),
    ThreadIds
  ),
  forall(
    member(ThreadId, ThreadIds),
    thread_join(ThreadId, true)
  ).

forall_thread_end(Topic, _Msg):-
  \+ debugging(Topic), !.
forall_thread_end(Topic, Msg):-
  thread_self(Id),
  thread_property(Id, status(Status)),
  (Status == true -> Alarm = '' ; Alarm = '[!!!ALARM!!!]'),
  debug(Topic, '~w~w exited with status ~w.', [Alarm,Msg,Status]).

print_thread(Alias):-
  thread_property(Id, alias(Alias)),
  thread_property(Id, status(Status)),
  write(Alias),tab(1),
  write(Status),nl.

print_threads:-
  % Print the threads in the alphabetical order of their alias.
  aggregate_all(
    set(Alias),
    thread_property(_, alias(Alias)),
    Aliases
  ),
  write('Alias'),tab(1),
  write('Status'),nl,
  maplist(print_thread, Aliases), !.

thread_create(Goal):-
  thread_create(Goal, _, []).



% RUN ON SUBLIST INFRASTRUCTURE %

%! intermittent_goal(:Goal, :EndGoal, +Interval:positive_integer) is det.
% Performs the given goal interspersed with time intervals
% of the given duration.
%
% If the end goal succeeds the thread succeeds
% (i.e., intermittent goal execution stops).

intermittent_goal(_G, EndG, _I):-
  call(EndG), !.
intermittent_goal(G, EndG, I):-
  call(G),
  sleep(I),
  intermittent_goal(G, EndG, I).

%! intermittent_thread(
%!   :Goal,
%!   :EndGoal,
%!   +Interval:positive_integer,
%!   -Id,
%!   +Options:list(nvpair)
%! ) is det.
% ...
%
% @arg Goal The goal that is repeated.
% @arg EndGoal The goal that stops `Goal` from being repeated,
%        as soon as it succeeds once.
% @arg Interval A positive integer representing the number of seconds
%        in between consecutive goal executions.
% @arg Id
% @arg Options A list of name-value pairs.

intermittent_thread(G, EndG, I, Id, O):-
  thread_create(intermittent_goal(G, EndG, I), Id, O).

%! run_on_sublists(+List, :Goal, +NumberOfThreads:positive_integer) is det.
% Run the given goal in different threads,
% on different sublists of the given list.

run_on_sublists(List, Mod:Goal, N):-
  split_list_by_number_of_sublists(List, N, Sublists),
  findall(
    ThreadId,
    (
      member(TaskList, Sublists),
      thread_start(Mod, Goal, TaskList, ThreadId)
    ),
    ThreadIds
  ),
  % Collect the threads after execution and display any failures to user.
  maplist(thread_join, ThreadIds, Statuses),
  exclude(==(true), Statuses, OopsStatuses),
  forall(
    member(OopsStatus, OopsStatuses),
    debug(thread_ext, '~w', [OopsStatus])
  ).

thread_alias(ThreadAlias):-
  nonvar(ThreadAlias), !,
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
  aggregate_all(
    set(ThreadAlias/Current/End),
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
      format(atom(Atom), '~w ~w {~w}', [ThreadAlias,ProgressBar,Status])
    ),
    Atoms
  ).

thread_overview_web(Markup):-
  thread_overview(Atoms),
  (
    Atoms == []
  ->
    Markup = [element(p,[],['No threads.'])]
  ;
    findall(
      element(pre, [], [Atom]),
      member(Atom, Atoms),
      Markup
    )
  ).

%! thread_prefix(+Prefix:atom, +Thread:atom) is semidet.
% Succeeds if the given thread name starts with the given prefix.
%! thread_prefix(+Prefix:atom, -Thread:atom) is nondet.
% Enumerates the currently running threads that start with the given prefix.

thread_prefix(Prefix, Thread):-
  thread_property(Thread, status(_)),
  atom_concat(Prefix, _, Thread).

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
  thread_alias(ThreadAlias),
  thread_create(call(Module:Goal, TaskList), _ThreadId, [alias(ThreadAlias)]),
  length(TaskList, NumberOfTasks),
  assert(end_flag(ThreadAlias, NumberOfTasks)),
  assert(workload(ThreadAlias, Module, Goal, TaskList)).

thread_status(ThreadAlias, Status):-
  thread_property(ThreadId, alias(ThreadAlias)),
  thread_property(ThreadId, status(Status)).

%! thread_success(+ThreadAlias:atom) is det.
% If the given atom is a registered thread alias,
% then the statistics for that running thread are
% upped by one.
%
% Notice that this predicate has to be called by
% the threaded goal, after each successful iteration
% inside goal.
%
% The thread alias is not passed around, by sould be
% retrieved inside the threaded goal using thread_self/1.

thread_success(ThreadAlias):-
  flag(ThreadAlias, ID, ID + 1),
  if_then(
    end_flag(ThreadAlias, ID),
    thread_end(ThreadAlias)
  ).
