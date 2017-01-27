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
    process_open/3,      % +Program, +In, -Out
    process_open/4,      % +Program, +In, +Args, -Out
    renice/2,            % +Pid:positive_integer, +Nice:between(-20,19)
    run_jar/2,           % +Jar, +Args
    run_jar/3,           % +Jar, +Args, :OutGoal_1
    run_jar/4,           % +Jar, +Args, :OutGoal_1, :ErrGoal_1
    run_jar/5,           % +Jar, +Args, :OutGoal_1, :ErrGoal_1, +Opts
    run_process/2,       % +Program, +Args
    run_process/3,       % +Program, +Args, :OutGoal_1
    run_process/4,       % +Program, +Args, :OutGoal_1, :ErrGoal_1
    run_process/5,       % +Program, +Args, :OutGoal_1, :ErrGoal_1, +Opts
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

@tbd Merge process_open/[3,4] into run_process/[2-5].

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
:- use_module(library(thread_ext)).
:- use_module(library(typecheck)).

:- at_halt(kill_processes).

%! os:pid_stream(
%!   ?Pid:positive_integer,
%!   ?StreamType:oneof([error,output]),
%!   ?ThreadId
%! ) is nondet.

:- dynamic
    os:pid_stream/3.

is_meta(error_goal).
is_meta(output_goal).

:- meta_predicate
    run_jar(+, +, :),
    run_jar(+, +, :, :),
    run_jar(+, +, :, :, +),
    run_process(+, +, :),
    run_process(+, +, :, :),
    run_process(+, +, :, :, +).

:- setting(
     tmp_dir,
     term,
     _,
     "Temporary directory."
   ).





%! exists_program(+Program) is semidet.
%
% Succeeds if the given program can be run from PATH.

exists_program(Program) :-
  var(Program), !,
  instantiation_error(Program).
exists_program(Program) :-
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
    image_dimensions0(File, Width, Height)
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

kill_processes :-
  % Make sure the PIDs are unique.
  % We do not want to kill the same process twice.
  with_mutex(process_id, (
    aggregate_all(set(Pid), process_id(Pid), Pids),
    concurrent_maplist(process_kill, Pids)
  )).



%! open_pdf(+File) is det.
%
% Opens the given PDF file.

open_pdf(File) :-
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

os(mac) :-
  current_prolog_flag(apple, true), !.
os(unix) :-
  current_prolog_flag(unix, true), !.
os(windows) :-
  current_prolog_flag(windows, true), !.



%! os_path(+File) is semidet.
%! os_path(-File) is nondet.
%
% Succeeds if File is on the OS path.

os_path(File) :-
  getenv('PATH', Path),
  os_path_separator(Sep),
  atomic_list_concat(Files, Sep, Path),
  member(File0, Files),
  prolog_to_os_filename(File, File0).



%! os_path_separator(+Separator) is semidet.
%! os_path_separator(-Separator) is det.
% Suceeds if Separator is the OS path separator character.

os_path_separator(Sep) :-
  os(Os),
  os_path_separator(Os, Sep).

%! os_path_separator(+Os:os, -Separator) is det.

os_path_separator(Os, Sep) :-
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



%! process_open(+Program, +In, +Out) is det.
%! process_open(+Program, +In, +Args, +Out) is det.

process_open(Program, In1, Out) :-
  process_open(Program, In1, [], Out).


process_open(Program, In1, Args, Out) :-
  process_create(
    path(Program),
    Args,
    [stdin(pipe(In2)),stdout(pipe(Out))]
    %%%%[stderr(pipe(Err)),stdin(pipe(In2)),stdout(pipe(Out))]
  ),
  set_stream(In2, type(binary)),
  call_cleanup(
    copy_stream_data(In1, In2),
    close(In2)
  ).
  % @bug Does not terminate for
  % http://lists.w3.org/Archives/Public/html-tidy/2011JulSep/0001.html
  %%%%call_cleanup(
  %%%%  (
  %%%%    read_stream_to_string(Err, Msg),
  %%%%    (Msg == "" -> true ; msg_warning(Msg))
  %%%%  ),
  %%%%  close(Err)
  %%%%).



%! renice(+Pid:positive_integer, +Nice:between(-20,19)) is det.

renice(Pid, N) :-
  with_mutex(process_id,(
    (   process_id(Pid)
    ->  must_be(between(-20,19), N),
        run_process(renice, [10,Pid])
    ;   existence_error(process, Pid)
    )
  )).



%! run_jar(+Jar, +Args) is det.
%! run_jar(+Jar, +Args, :OutGoal_1) is det.
%! run_jar(+Jar, +Args, :OutGoal_1, :ErrGoal_1) is det.
%! run_jar(+Jar, +Args, :OutGoal_1, :ErrGoal_1, +Opts) is det.
%
% Runs the given JAR file with the given commandline arguments.
%
% Options are passed to run_process/5.

run_jar(Jar, Args) :-
  run_jar(Jar, Args, true).


run_jar(Jar, Args, OutGoal_1) :-
  run_jar(Jar, Args, OutGoal_1, process_error, []).


run_jar(Jar, Args, OutGoal_1, ErrGoal_1) :-
  run_jar(Jar, Args, OutGoal_1, ErrGoal_1, []).


run_jar(Jar, Args, OutGoal_1, ErrGoal_1, Opts) :-
  run_process(java, ['-jar',file(Jar)|Args], OutGoal_1, ErrGoal_1, Opts).



%! run_process(+Program, +Args) is det.
%! run_process(+Program, +Args, :OutGoal_1) is det.
%! run_process(+Program, +Args, :OutGoal_1, :ErrGoal_1) is det.
%! run_process(+Program, +Args, :OutGoal_1, :ErrGoal_1, +Opts) is det.
%
% Calls an external process with the given name and arguments, and
% calls an internal goal on the process' output.
%
% The following options are supported:
%
%   * status(-nonneg)
%
%   * Other options are passed to process_create/3.
%
% @tbd Run output and error goals is separate threads.
%
% ```
% process_create(path(cat), [file('a.txt'),file('b.txt')], [stdout(pipe(Out))]),
% copy_stream_data(Out, user_output).
%
% run_process(
%   cat,
%   [file('a.txt'),file('b.txt')],
%   [Out]>>copy_stream_data(Out,current_output)
% )
% ```

run_process(Program, Args) :-
  run_process(Program, Args, true).


run_process(Program, Args, OutGoal_1) :-
  run_process(Program, Args, OutGoal_1, process_error).


run_process(Program, Args, OutGoal_1, ErrGoal_1) :-
  run_process(Program, Args, OutGoal_1, ErrGoal_1, []).


run_process(Program, Args, OutGoal_1, ErrGoal_1, Opts0) :-
  meta_options(is_meta, Opts0, Opts),
  include(process_create_option, Opts, ProcessOpts1),
  merge_options(
    [process(Pid),stderr(pipe(Err)),stdout(pipe(Out))],
    ProcessOpts1,
    ProcessOpts2
  ),
  % Program is either an absolute file or a related file that is
  % resolved WRT the PATH.
  (is_absolute_file_name(Program) -> Exec = Program ; Exec = path(Program)),
  setup_call_cleanup(
    process_create(Exec, Args, ProcessOpts2),
    (
      thread_create(
        call(ErrGoal_1, Err),
        ErrId,
        % @tbd Option debug?
        [at_exit(close(Err)),debug(false)]
      ),
      assert(os:pid_stream(Pid,error,ErrId)),
      call(OutGoal_1, Out),
      process_wait(Pid, exit(OutStatus)),
      process_status(OutStatus),
      ignore(option(status(OutStatus), Opts))
    ),
    (
      close(Out),
      % Make sure the streams have been fully processed.
      (   retract(os:pid_stream(Pid,error,ErrId))
      ->  thread_join(ErrId, ErrStatus),
          thread_status(Pid, error, ErrStatus)
      ;   true
      )
    )
  ).

process_error(In) :-
  read_stream_to_codes(In, Cs, []),
  (   Cs == []
  ->  true
  ;   string_codes(Str, Cs),
      print_message(warning, Str)
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
process_status(exit(StatusCode)) :- !,
  process_status(StatusCode).
% Success code.
process_status(0) :- !.
% Error/exception code.
process_status(StatusCode) :-
  print_message(warning, process_status(StatusCode)).

%! thread_status(
%!   +Pid:positive_integer,
%!   +StreamType:oneof([error,output]),
%!   +Status:compound
%! ) is det.

thread_status(_, _, true) :- !.
thread_status(Pid, StreamType, false) :- !,
  print_message(warning, thread_status(Pid,StreamType,fail,_)).
thread_status(Pid, StreamType, exception(Exception)) :- !,
  print_message(warning, thread_status(Pid,StreamType,exception,Exception)).
thread_status(Pid, StreamType, exited(Term)) :-
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

parse_wc0(N1, N2, N3, In) :-
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
