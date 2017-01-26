:- module(
  os_ext,
  [
    exists_program/1,    % +Program
    image_dimensions/3,  % +File, -Width, -Height
    open_pdf/1,          % +File
    os/1,                % ?Os
    os_path/1,           % ?File
    os_path_separator/1, % ?Sep
    os_root_prefix/1,    % ?Prefix
    renice/2,            % +Pid:positive_integer, +Nice:between(-20,19)
    run_jar/2,           % +Jar, +Args
    run_jar/3,           % +Jar, +Args, +Opts
    run_process/2,       % +Program, +Args
    run_process/3,       % +Program, +Args, +Opts
    sort_file/1,         % +File
    sort_file/2,         % +File, +Opts
    wc/2,                % +File, -NumLines
    wc/4                 % +File, -NumLines, -NumWords, -NumBytes
  ]
).

/** <module> OS extensions

Support for using external programs and other OS functions.

Automatically kill all running processes upon halt.

Process output and error streams in parallel by using threads.

@author Wouter Beek
@version 2015/08-2017/01
*/

:- use_module(library(aggregate)).
:- use_module(library(ansi_term)).
:- use_module(library(apply)).
:- use_module(library(cli_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(io)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(settings)).
:- use_module(library(thread)).
:- use_module(library(typecheck)).

:- at_halt(kill_processes).

%! thread_id(?Pid:nonneg, ?StreamType:oneof([error,output]), ?ThreadId) is nondet.

:- dynamic
    os:thread_id/3.

is_meta(error_goal).
is_meta(output_goal).

:- meta_predicate
    run_jar(+, +, :),
    run_process(+, +, :).

:- setting(
     tmp_dir,
     term,
     _,
     "Temporary directory."
   ).





%! exists_program(+Program) is semidet.
%
% Succeeds if the given program can be run from PATH.

exists_program(Program):-
  var(Program), !,
  instantiation_error(Program).
exists_program(Program):-
  os_path(Prefix),
  atomic_list_concat([Prefix,Program], /, Exe),
  access_file(Exe, execute), !.



%! image_dimensions(+File, -Width:float, -Height:float) is det.
%
% @see Requires ImageMagick.

image_dimensions(File, Width, Height) :-
  run_process(
    identify,
    [file(File)],
    [output_goal(image_dimensions0(File, Width, Height))]
  ).

image_dimensions0(File, Width, Height, In) :-
  read_stream_to_codes(In, Cs),
  phrase(image_dimensions0(File, Width, Height), Cs).

image_dimensions0(File, Width, Height) -->
  atom(File),
  " ",
  ...,
  " ",
  integer(Width),
  "x",
  integer(Height),
  done.



%! kill_processes is det.
%
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



%! open_pdf(+File) is det.
%
% Opens the given PDF file.
%
% Options are passed to run_process/3.

open_pdf(File):-
  once((
    member(Program, [xpdf,evince]),
    exists_program(Program)
  )),
  run_process(Program, [file(File)]).



%! os(+Os) is semidet.
%! os(-Os) is det.
%
% Succeeds if Os names the current Operating System.
%
% Supported values are:
%   * mac
%   * unix
%   * windows

os(mac):-
  current_prolog_flag(apple, true), !.
os(unix):-
  current_prolog_flag(unix, true), !.
os(windows):-
  current_prolog_flag(windows, true), !.



%! os_path(+File) is semidet.
%! os_path(-File) is nondet.
%
% Succeeds if File is on the OS path.

os_path(File):-
  getenv('PATH', Path),
  os_path_separator(Sep),
  atomic_list_concat(Files, Sep, Path),
  member(File0, Files),
  prolog_to_os_filename(File, File0).



%! os_path_separator(+Separator) is semidet.
%! os_path_separator(-Separator) is det.
% Suceeds if Separator is the OS path separator character.

os_path_separator(Sep):-
  os(Os),
  os_path_separator(Os, Sep).

%! os_path_separator(+Os:os, -Separator) is det.

os_path_separator(Os, Sep):-
  os(Os),
  (   memberchk(Os, [mac,unix])
  ->  Sep = (:)
  ;   Os == windows
  ->  Sep = (;)
  ).



%! os_root_prefix(+Prefix) is semidet.
%! os_root_prefix(-Prefix) is multi.

:- if(os(unix)).
os_root_prefix(/).
:- endif.
:- if(os(windows)).
os_root_prefix('C:\\').
:- endif.



%! renice(+Pid:positive_integer, +Nice:between(-20,19)) is det.

renice(Pid, _):-
  \+ process_id(Pid), !,
  existence_error(process, Pid).
renice(Pid, N):-
  must_be(between(-20,19), N),
  run_process(renice, [10,Pid], [nice(0)]).



%! run_jar(+Jar, +Args) is det.
%! run_jar(+Jar, +Args, +Opts) is det.
%
% Runs the given JAR file with the given commandline arguments.
%
% Options are passed to run_process/3.

run_jar(Jar, Args):-
  run_jar(Jar, Args, []).


run_jar(Jar, Args, Opts0):-
  meta_options(is_meta, Opts0, Opts),
  run_process(java, ['-jar',file(Jar)|Args], Opts).



%! run_process(+Program, +Args) is det.
%! run_process(+Program, +Args, +Opts) is det.
%
% Calls an external process with the given name and arguments, and
% call an internal goal on the output that is coming from the external
% process.
%
% The following options are supported:
%
%   * detached(+boolean)
%
%     Whether to run the process in a detached thread or not.  Default
%     is `false`.
%
%   * error_goal(+callable)
%
%     Call this goal on the error stream.
%
%   * output_goal(+callable)
%
%     Call this goal on the output stream.
%
%   * status(-nonneg)
%
%   * Other options are passed to process_create/3.
%
% @tbd Output and error is separate threads.

run_process(Program, Args):-
  run_process(Program, Args, []).


run_process(Program, Args, Opts1):-
  meta_options(is_meta, Opts1, Opts2),
  (   select_option(detached(true), Opts2, Opts3)
  ->  thread_create(
        run_process_inner(Program, Args, Opts3),
        _,
        [detached(true)]
      )
  ;   run_process_inner(Program, Args, Opts2)
  ).

run_process_inner(Program, Args, Opts1):-
  % Make sure the PID can be returned as an option, if given.
  (   select_option(process(Pid), Opts1, Opts2)
  ->  true
  ;   Opts2 = Opts1
  ),
  
  % Filter out those options that can be processed by
  % process_create/3.
  include(process_create_option, Opts2, Opts3),
  
  % Make sure that the options for the PID, stderr and stdout are
  % always handled to process_create/3.
  merge_options(
    Opts3,
    [process(Pid),stderr(pipe(ErrorStream)),stdout(pipe(OutputStream))],
    Opts4
  ),
  
  % Allow process to be either an absolute file or a file relative to
  % path.
  (   is_absolute_file_name(Program)
  ->  Exec = Program
  ;   Exec = path(Program)
  ),
  
  setup_call_cleanup(
    process_create(Exec, Args, Opts4),
    (
      % Program the goal supplied for the error stream.
      (   option(error_goal(ErrorGoal), Opts2, print_error)
      ->  thread_create(
            call(ErrorGoal, ErrorStream),
            ErrorThreadId,
            [at_exit(close(ErrorStream)),debug(false)]
          ),
          assert(thread_id(Pid,error,ErrorThreadId))
      ;   true
      ),

      % Program the goal supplied for the output stream.
      (   option(output_goal(OutputGoal), Opts2)
      ->  call(OutputGoal, OutputStream)
      ;   true
      ),

      % Process the status code.
      with_mutex(process_ext, (
        process_wait(Pid, exit(PidStatus))
      )),
      process_status(PidStatus),
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

%! process_status(+Status:or([compound,nonneg])) is det.
%
% Handling of exit codes given by programs that are run from the
% shell.
%
% @arg Status Either an integer status code, or an integer status code
%      wrapped in functor `exit`.
%
% @throws shell_error Throws a shell error when a shell process exits
%         with a non-zero code.

% Unwrap code.
process_status(exit(StatusCode)):- !,
  process_status(StatusCode).
% Success code.
process_status(0):- !.
% Error/exception code.
process_status(StatusCode):-
  print_message(warning, process_status(StatusCode)).

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



%! sort_file(+File) is det.
%! sort_file(+File, +Opts) is det.
%
% The following options are supported:
%
%   * buffer_size(+nonneg)
%
%     Optionally, the size of the buffer in kilobytes.
%
%   * duplicates(+boolean)
%
%     Whether duplicates are allowed in the result.  Default is
%     `true`.
%
%   * numeric(+boolean)
%
%     Whether numberic sort is performed.  Default is `false`.
%
%   * output(+atom)
%
%     The name of the output file.  Default is the input file.
%
%   * tmp_dir(+atom)
%
%     The directory that is used for temporary sort results.  Default
%     is File's directory.
%
%   * threads(+positive_integer)
%
%     The number of threads that is used.  Default is the number of
%     available processors, but not larger than 8.  Larger numbers
%     have diminishing returns.  Using $n$ threads increases the
%     memory use by $\log n$.  Default is 1.
%
%   * utf8(+boolean)
%
%     Whether the environment is set to UTF-8 encoding.  Default is
%     `true`.

sort_file(File) :-
  setting(tmp_dir, Dir),
  (var(Dir) -> file_directory_name(File, Dir) ; true),
  sort_file(File, [duplicates(false),tmp_dir(Dir)]).


sort_file(File1, Opts1) :-
  must_be_file(read, File1),
  % The UTF-8 encoding option is handled by an environment variable.
  (option(utf8(true), Opts1, true) -> Env = [] ; Env = ['LC_ALL'='C']),
  (   option(output(_), Opts1)
  ->  Opts2 = Opts1
  ;   file_name_extension(File1, gv, File2),
      merge_options([output(File2)], Opts1, Opts2)
  ),
  sort_file_args(Opts2, Args),
  maplist(term_to_atom, Args, T),
  atomic_list_concat([sort,File1|T], ' ', Msg),
  format(user_output, "<<<~w~n", [Msg]),
  process_create(path(sort), [file(File1)|Args], [env(Env)]),
  format(user_output, "~w>>>~n", [Msg]).

sort_file_args([], []).
sort_file_args([buffer_size(Size)|T1], [Arg|T2]) :- !,
  long_flag('buffer-size', Size, Arg),
  sort_file_args(T1, T2).
sort_file_args([duplicates(false)|T1], ['--unique'|T2]) :- !,
  sort_file_args(T1, T2).
sort_file_args([numeric(Numeric)|T1], [Arg|T2]) :- !,
  must_be(boolean, Numeric),
  long_flag('numeric-sort', Arg),
  sort_file_args(T1, T2).
sort_file_args([output(File)|T1], [Arg|T2]) :- !,
  must_be_file(write, File),
  long_flag(output, File, Arg),
  sort_file_args(T1, T2).
sort_file_args([threads(Threads)|T1], [Arg|T2]) :- !,
  long_flag(parallel, Threads, Arg),
  sort_file_args(T1, T2).
sort_file_args([tmp_dir(Dir)|T1], [Arg|T2]) :- !,
  must_be_directory(Dir),
  long_flag('temporary-directory', Dir, Arg),
  sort_file_args(T1, T2).
sort_file_args([_|T1], L2) :-
  sort_file_args(T1, L2).



%! wc(+File, -NumLines) is det.
%! wc(+File, -NumLines, -NumWords, -NumBytes) is det.

wc(File, N1) :-
  wc(File, N1, _, _).


wc(File, N1, N2, N3) :-
  run_process(wc, [file(File)], [output_goal(parse_wc0(N1,N2,N3))]).

parse_wc0(N1, N2, N3, In):-
  read_stream_to_codes(In, Cs),
  phrase(wc0(N1, N2, N3), Cs, _).

% Example:
% ```bash
% 427  1818 13512 README.md
% ```
wc0(N1, N2, N3) -->
  whites, integer(N1),
  whites, integer(N2),
  whites, integer(N3).





% MESSAGES %

:- multifile
    prolog:message//1.

prolog:message(process_status(StatusCode)) -->
  ["Process returned status code ~w."-[StatusCode]].
prolog:message(thread_status(Pid,StreamType,fail)) -->
  ["The ~a stream of process ~d failed."-[StreamType,Pid]].
prolog:message(thread_status(Pid,StreamType,exception,Term)) -->
  ["The ~a stream of process ~d threw exception ~w "-[StreamType,Pid,Term]].
prolog:message(thread_status(Pid,StreamType,exited,Term)) -->
  ["The ~a stream of process ~d exited with ~w "-[StreamType,Pid,Term]].
