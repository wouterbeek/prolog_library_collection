:- module(
  process_ext,
  [
    renice/2,      % +Pid:positive_integer, +Nice:between(-20,19)
    run_jar/2,     % +Jar:atom, +Args
    run_jar/3,     % +Jar:atom, +Args, +Opts
    run_process/2, % +Process:atom, +Args
    run_process/3  % +Process:atom, +Args, +Opts
  ]
).

:- reexport(library(process)).

/** <module> Process extensions

Additional support for starting processes from within SWI-Prolog:
  * Automatically kill all running processes upon halt.
  * Process output and error streams in parallel by using threads.

@author Wouter Beek
@version 2015/08, 2015/10, 2016/03
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(thread)).

%! thread_id(
%!   ?Pid:nonneg,
%!   ?StreamType:oneof([error,output]),
%!   ?ThreadId:atom
%! ) is nondet.

:- dynamic
    thread_id/3.

:- predicate_options(run_jar/3, 3, [
     pass_to(handle_process/3, 3)
   ]).
:- predicate_options(run_process/3, 3, [
     detached(+boolean),
     pass_to(run_process_inner/3, 3)
   ]).
:- predicate_options(run_process_inner/3, 3, [
     error_goal(+callable),
     output_goal(+callable),
     program(+atom),
     status(-nonneg),
     pass_to(process_create/3, 3)
   ]).

:- meta_predicate
    run_jar(+, +, :),
    run_process(+, +, :).

is_meta(error_goal).
is_meta(output_goal).

:- at_halt(kill_processes).





%! renice(+Pid:positive_integer, +Nice:between(-20,19)) is det.

renice(Pid, _):-
  \+ process_id(Pid), !,
  existence_error(process, Pid).
renice(Pid, N):-
  must_be(between(-20,19), N),
  run_process(renice, [10,Pid], [nice(0)]).



%! run_jar(+Jar:atom, +Args) is det.
%! run_jar(+Jar:atom, +Args, +Opts) is det.
% Runs the given JAR file with the given commandline arguments.
%
% Options are passed to run_process/3.

run_jar(Jar, Args):-
  run_jar(Jar, Args, []).

run_jar(Jar, Args, Opts1):-
  meta_options(is_meta, Opts1, Opts2),

  % Set the program option.
  file_base_name(Jar, JarName),
  format(atom(Program), 'Java/JAR ~a', [JarName]),
  merge_options([program(Program)], Opts2, Opts3),

  run_process(java, ['-jar',file(Jar)|Args], Opts3).



%! run_process(+Process:atom, +Args:list) is det.
%! run_process(+Process:atom, +Args:list, +Opts) is det.
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
%   * output_goal(:Goal)
%     Call this goal on the output stream.
%   * program(+Program:atom)
%     The name of the program as displayed in debug messages.
%     Default is Process.
%   * status(-Status:nonneg)
%   * Other options are passed to process_create/3.
%
% @tbd Output and error is separate threads.

run_process(Process, Args):-
  run_process(Process, Args, []).


run_process(Process, Args, Opts1):-
  meta_options(is_meta, Opts1, Opts2),
  (   select_option(detached(true), Opts2, Opts3)
  ->  thread_create(
        run_process_inner(Process, Args, Opts3),
        _,
        [detached(true)]
      )
  ;   run_process_inner(Process, Args, Opts2)
  ).

run_process_inner(Process, Args, Opts1):-
  % By default the program name is the process name.
  option(program(Program), Opts1, Process),
  
  % Make sure the PID can be returned as an option, if given.
  (   select_option(process(Pid), Opts1, Opts2)
  ->  true
  ;   Opts2 = Opts1
  ),
  
  % Filter out those options that can be processed by process_create/3.
  include(process_create_option, Opts2, Opts3),
  
  % Make sure that the options for the PID, standard error
  % and standard output are always handled to process_create/3.
  merge_options(
    Opts3,
    [process(Pid),stderr(pipe(ErrorStream)),stdout(pipe(OutputStream))],
    Opts4
  ),
  
  % Allow process to be either an absolute file or a file
  % relative to path.
  (   is_absolute_file_name(Process)
  ->  Exec = Process
  ;   Exec = path(Process)
  ),
  
  setup_call_cleanup(
    process_create(Exec, Args, Opts4),
    (
      % Process the goal supplied for the error stream.
      (   option(error_goal(ErrorGoal), Opts2, print_error)
      ->  thread_create(
            call(ErrorGoal, ErrorStream),
            ErrorThreadId,
            [at_exit(close(ErrorStream)),debug(false)]
          ),
          assert(thread_id(Pid,error,ErrorThreadId))
      ;   true
      ),

      % Process the goal supplied for the output stream.
      (   option(output_goal(OutputGoal), Opts2)
      ->  call(OutputGoal, OutputStream)
      ;   true
      ),

      % Process the status code.
      with_mutex(process_ext, (
        process_wait(Pid, exit(PidStatus))
      )),
      process_status(Program, PidStatus),
      (   option(status(PidStatus0), Opts2)
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



%! kill_processes is det.
% Kills all running processes by PID.

kill_processes:-
  % Make sure the PIDs are unique.
  % We do not want to kill the same process twice.
  with_mutex(process_ext, (
    aggregate_all(
      set(Pid),
      process_id(Pid),
      Pids
    ),
    concurrent_maplist(process_kill, Pids)
  )).





% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(process_status(Program,StatusCode)) -->
  ['Program ~w returned status code ~w.'-[Program,StatusCode]].
prolog:message(thread_status(Pid,StreamType,fail)) -->
  ['The ~a stream of process ~d failed.'-[StreamType,Pid]].
prolog:message(thread_status(Pid,StreamType,exception,Term)) -->
  ['The ~a stream of process ~d threw exception ~w '-[StreamType,Pid,Term]].
prolog:message(thread_status(Pid,StreamType,exited,Term)) -->
  ['The ~a stream of process ~d exited with ~w '-[StreamType,Pid,Term]].
