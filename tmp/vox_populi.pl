:- module(
  vox_populi,
  [
    ask_thread/3, % +Thread:atom
                  % +Question:compound
                  % -Answer
    ask_thread_prefix/3, % +ThreadPrefix:atom
                         % +Question:compound
                         % -Answers:list
    ask_threads/3, % +Threads:list(atom)
                   % +Question:compound
                   % -Answers:list
    command_thread/2, % +Thread:atom
                      % +Command:compound
    command_thread_prefix/2, % +ThreadPrefix:atom
                             % +Command:compound
    command_threads/2 % +Threads:list(atom)
                      % +Command:compound
  ]
).

/** <module> Vox Populi

@author Wouter Beek
@version 2014/06
*/

:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(process/thread_ext)).



%! ask_thread(+Thread:atom, +Question:compound, -Answer) is det.

ask_thread(Thread, Question, Answer):-
  ask_threads([Thread], Question, [Answer]).


%! ask_thread_prefix(
%!   +ThreadPrefix:atom,
%!   +Question:compound,
%!   -Answers:list
%! ) is det.

ask_thread_prefix(ThreadPrefix, Question, Answers):-
  findall(
    Thread,
    thread_prefix(ThreadPrefix, Thread),
    Threads
  ),
  ask_threads(Threads, Question, Answers).


%! ask_threads(+Threads:list(atom), +Question:compound, -Answers:list) is det.

ask_threads(Threads, Question, Answers):-
  thread_create(ask_threads0(Threads, Question, Answers), ThreadId, []),
  thread_join(ThreadId, exited(Answers)).

ask_threads0(Threads, Question, Answers):-
  thread_self(Me),
  forall(
    member(Thread, Threads),
    thread_send_message(Thread, question(Me,Question))
  ),
  collect_answers(Threads, Answers),
  thread_exit(Answers).

%! collect_answers(+Threads:list(atom), -Answers:list) is det.

collect_answers(Threads, Answers):-
  collect_answers(Threads, [], Answers).

collect_answers([], Solution, Solution):- !.
collect_answers(Threads1, Answers, Solution):-
  thread_get_message(answer(Thread,Answer)),
  selectchk(Thread, Threads1, Threads2), !,
  collect_answers(Threads2, [Answer|Answers], Solution).
collect_answers(Threads, Answers, Solution):-
  sleep(1),
  collect_answers(Threads, Answers, Solution).


%! command_thread(+Thread:atom, +Command:compound) is det.

command_thread(Thread, Command):-
  command_threads([Thread], Command).


%! command_thread_prefix(+ThreadPrefix:atom, +Command:compound) is det.

command_thread_prefix(Prefix, Command):-
  findall(
    Thread,
    thread_prefix(Prefix, Thread),
    Threads
  ),
  command_threads(Threads, Command).


%! command_threads(+Threads:list(atom), +Command:compound) is det.

command_threads(Threads, Command):-
  thread_self(Me),
  forall(
    member(Thread, Threads),
    thread_send_message(Thread, command(Me,Command))
  ).

