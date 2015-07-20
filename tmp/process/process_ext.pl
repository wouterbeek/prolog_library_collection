:- module(
  process_ext,
  [
    handle_process/3 % +Process:atom
                     % +Arguments:list
                     % +Options:list(nvpair)
  ]
).

/** <module> Process extensions

@author Wouter Beek
@version 2015/01, 2015/03-2015/04
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(thread)).

% Processes are registered so that they can be killed.
:- dynamic(current_process/1).

%! thread_id(
%!   ?Pid:nonneg,
%!   ?StreamType:oneof([error,output]),
%!   ?ThreadId:atom
%! ) is nondet.
:- dynamic(thread_id/3).

:- predicate_options(handle_process/3, 3, [
  detached(+boolean),
  pass_to(handle_process_inner/3, 3)
]).
:- predicate_options(handle_process_inner/3, 3, [
  error_goal(+callable),
  nice(+between(-20,20)),
  output_goal(+callable),
  program(+atom),
  status(-nonneg),
  pass_to(process_create/3, 3)
]).

:- meta_predicate(handle_process(+,+,:)).

is_meta(error_goal).
is_meta(output_goal).

:- at_halt(kill_processes).





%! handle_process(
%!   +Process:atom,
%!   +Arguments:list,
%!   +Options:list(nvpair)
%! ) is det.
% Calls an external process with the given name and arguments,
% and call an internal goal on the output that is coming from
% the external process.
%
% The following options are supported:
%   * detached(+boolean)
%     Whether to run the process in a detached thread or not.
%     Default is `false`.
%   * error_goal(:Goal)
%     Call this goal on the error stream.
%   * nice(+between(-20,20))
%     Default is 0.
%   * output_goal(:Goal)
%     Call this goal on the output stream.
%   * program(+Program:atom)
%     The name of the program as displayed in debug messages.
%     Default is Process.
%   * status(-Status:nonneg)
%   * Other options are passed to process_create/3.
%
% @tbd Output and error is separate threads.

handle_process(Process, Args, Options1):-
  meta_options(is_meta, Options1, Options2),
  (   select_option(detached(true), Options2, Options3)
  ->  thread_create(
        handle_process_inner(Process, Args, Options3),
        _,
        [detached(true)]
      )
  ;   handle_process_inner(Process, Args, Options2)
  ).

handle_process_inner(Process, Args, Options1):-
  option(program(Program), Options1, Process),
  include(process_create_option, Options1, Options2),
  merge_options(
    Options2,
    [process(Pid),stderr(pipe(ErrorStream)),stdout(pipe(OutputStream))],
    Options3
  ),
  (   is_absolute_file_name(Process)
  ->  Exec = Process
  ;   Exec = path(Process)
  ),
  setup_call_cleanup(
    process_create(Exec, Args, Options3),
    (
      (   option(nice(Nice), Options1),
          Nice =\= 0
      ->  handle_process(renice, [10,Pid], [nice(0)])
      ;   true
      ),

      % Register the PID so it can be killed upon Prolog halt.
      assert(current_process(Pid)),

      % Process the goal supplied for the error stream.
      (   option(error_goal(ErrorGoal), Options1, print_error)
      ->  thread_create(
            call(ErrorGoal, ErrorStream),
            ErrorThreadId,
            [at_exit(close(ErrorStream)),debug(false)]
          ),
          assert(thread_id(Pid,error,ErrorThreadId))
      ;   true
      ),

      % Process the goal supplied for the output stream.
      (   option(output_goal(OutputGoal), Options1)
      ->  call(OutputGoal, OutputStream)
      ;   true
      ),

      % Process the status code.
      with_mutex(process_ext, (
        process_wait(Pid, exit(PidStatus)),
        retract(current_process(Pid))
      )),
      process_status(Program, PidStatus),
      (   option(status(PidStatus0), Options1)
      ->  PidStatus0 = PidStatus
      ;   true
      )
    ),
    (
      close(OutputStream),
      % Make sure the streams have been fully processed.
      (   retract(thread_id(Pid,error,ErrorThreadId))
      ->  thread_join(ErrorThreadId, ErrorThreadStatus),
          thread_status(Pid,error,ErrorThreadStatus)
      ;   true
      )
    )
  ).

print_error(In):-
  read_stream_to_codes(In, Codes, []),
  (   Codes == []
  ->  true
  ;   string_codes(String, Codes),
      print_message(warning, String)
  ).

process_create_option(cwd(_)).
process_create_option(detached(_)).
process_create_option(env(_)).
process_create_option(priority(_)).
process_create_option(window(_)).

%! process_status(+Program, +Status:or([compound,nonneg])) is det.
% Handling of exit codes given by programs that are run from the shell.
%
% @arg Program
% @arg Status Either an integer status code,
%      or an integer status code wrapped in functor `exit`.
%
% @throws shell_error Throws a shell error when a shell process exits with
%         a non-zero code.

% Unwrap code.
process_status(Program, exit(StatusCode)):- !,
  process_status(Program, StatusCode).
% Success code.
process_status(_, 0):- !.
% Error/exception code.
process_status(Program, StatusCode):-
  print_message(warning, process_status(Program,StatusCode)).

%! thread_status(
%!   +Pid:nonneg,
%!   +StreamType:oneof([error,output]),
%!   +Status:compound
%! ) is det.

thread_status(_, _, true):- !.
thread_status(Pid, StreamType, false):- !,
  print_message(warning, thread_status(Pid,StreamType,fail,_)).
thread_status(Pid, StreamType, exception(Exception)):- !,
  print_message(warning, thread_status(Pid,StreamType,exception,Exception)).
thread_status(Pid, StreamType, exited(Term)):-
  print_message(warning, thread_status(Pid,StreamType,exited,Term)).

:- multifile(prolog:message//1).

prolog:message(process_status(Program,StatusCode)) -->
  ['Program ~w returned status code ~w.'-[Program,StatusCode]].
prolog:message(thread_status(Pid,StreamType,fail)) -->
  ['The ~a stream of process ~d failed.'-[StreamType,Pid]].
prolog:message(thread_status(Pid,StreamType,exception,Term)) -->
  ['The ~a stream of process ~d threw exception ~w '-[StreamType,Pid,Term]].
prolog:message(thread_status(Pid,StreamType,exited,Term)) -->
  ['The ~a stream of process ~d exited with ~w '-[StreamType,Pid,Term]].



%! kill_processes is det.
% Kills all running processes by PID.

kill_processes:-
  % Make sure the PIDs are unique.
  % We do not want to kill the same process twice.
  with_mutex(process_ext, (
    aggregate_all(
      set(Pid),
      current_process(Pid),
      Pids
    ),
    concurrent_maplist(process_kill, Pids)
  )).

