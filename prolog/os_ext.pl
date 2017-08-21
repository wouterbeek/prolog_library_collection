:- module(
  os_ext,
  [
    process_flags/3, % :Goal_2, +Args, -Flags
    process_open/3,  % +Program, +In, -Out
    process_open/4,  % +Program, +In, +Args, -Out
    process_open/5,  % +Program, +In, +Args, -Out, +Options
    run_jar/3,       % +Jar, +Args, :Goal_1
    run_jar/4,       % +Jar, +Args, :Goal_1, +Options
    run_process/2,   % +Program, +Args
    run_process/3,   % +Program, +Args, +Options
    run_process/4    % +Program, +Args, :Goal_1, +Options
  ]
).

/** <module> OS extensions

@author Wouter Beek
@version 2017/04-2017/08
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(hash_ext)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(yall)).

:- meta_predicate
    process_flags(2, +, -),
    run_jar(+, +, 1),
    run_jar(+, +, 1, +),
    run_process(+, +, 1, +).





%! process_flags(:Goal_2, +Args:list(compound), -Flags:list(atom)) is det.

process_flags(_, [], []).
process_flags(Goal_2, [H1|T1], [H2|T2]) :-
  call(Goal_2, H1, H2), !,
  process_flags(Goal_2, T1, T2).
process_flags(Goal_2, [_|T], L) :-
  process_flags(Goal_2, T, L).



%! process_open(+Program:atom, +In:stream, -Out:stream) is det.
%! process_open(+Program:atom, +In:stream, +Args:list(term),
%!              -Out:stream) is det.
%! process_open(+Program:atom, +In:stream, +Args:list(term), -Out:stream,
%!              +Options:list(compound)) is det.
%
% In is closed by this predicate.

process_open(Program, In, Out) :-
  process_open(Program, In, [], Out).


process_open(Program, In, Args, Out) :-
  process_open(Program, In, Args, Out, []).


process_open(Program, In, Args, ProcOut, Options1) :-
  process_options(Options1, Options2),
  merge_options(
    [stderr(pipe(ProcErr)),stdin(pipe(ProcIn)),stdout(pipe(ProcOut))],
    Options2,
    Options3
  ),
  process_debug(Program, Args),
  process_create(path(Program), Args, Options3),

  % Process input should have the same type as input.
  stream_property(In, type(Type)),
  (Type == text -> true ; set_stream(ProcIn, type(Type))),

  %forall(stream_property(In, P), writeln(P)),
  %peek_string(In, 100, Peek), writeln(Peek),
  %copy_stream_data(In, user_output),
  thread_create(copy_and_close(In, ProcIn), _, [detached(true)]),
  thread_create(print_error(ProcErr), _, [detached(true)]).

copy_and_close(In, Out) :-
  call_cleanup(
    copy_stream_data(In, Out),
    close(Out)
  ).



%! run_jar(+Jar:atom, +Args:list(term), :Goal_1) is det.
%! run_jar(+Jar:atom, +Args:list(term), :Goal_1,
%!         +Options:list(compound)) is det.

run_jar(Jar, Args, Goal_1) :-
  run_jar(Jar, Args, Goal_1, []).


run_jar(Jar, Args, Goal_1, Options) :-
  run_process(java, ['-jar',file(Jar)|Args], Goal_1, Options).



%! run_process(+Program:atom, +Args:list(term)) is det.
%! run_process(+Program:atom, +Args:list(term), :Goal_1) is det.
%! run_process(+Program:atom, +Args:list(term), :Goal_1,
%!             +Options:list(compound)) is det.
%
% ```prolog
% process_create(
%   path(cat),
%   [file('a.txt'),file('b.txt')],
%   [stdout(pipe(Out))]
% ),
% copy_stream_data(Out, user_output).
%
% run_process(
%   cat,
%   [file('a.txt'),file('b.txt')],
%   [Out]>>copy_stream_data(Out,current_output)
% )
% ```

run_process(Program, Args) :-
  run_process(Program, Args, []).


run_process(Program, Args, Options) :-
  run_process(
    Program,
    Args,
    [Out]>>copy_stream_data(Out, user_output),
    Options
  ).


run_process(Program, Args, Goal_1, Options1) :-
  process_options(Options1, Options2),
  merge_options(
    Options2,
    [process(Pid),stderr(pipe(Err)),stdout(pipe(Out))],
    Options3
  ),
  % `Program' is either an absolute file or a related file that is
  % resolved WRT the PATH environment variable.
  (is_absolute_file_name(Program) -> Exec = Program ; Exec = path(Program)),
  process_debug(Program, Args),
  setup_call_cleanup(
    process_create(Exec, Args, Options3),
    (
      call(Goal_1, Out),
      thread_create(print_error(Err), _, [detached(true)]),
      process_wait(Pid, exit(OutStatus)),
      process_status(OutStatus)
    ),
    close(Out)
  ).

process_debug(Program, Args) :-
  debugging(process_open), !,
  Goal_0 =.. [Program|Args],
  debug(process_open, "~w", [Goal_0]).
process_debug(_, _).

process_status(0) :- !.
process_status(exit(Status)) :-
  print_message(warning, process_status(Status)).





% HELPERS %

%! print_error(+Error:stream) is det.

print_error(Err) :-
  call_cleanup(
    print_errors(Err),
    close(Err)).

print_errors(Stream) :-
  read_line_to_string(Stream, String),
  (   String == end_of_file
  ->  true
  ;   print_message(warning, process_error(String)),
      print_errors(Stream)
  ).



%! process_options(+Options1:list(compound), -Options2:list(compound)) is det.

process_options([], []) :- !.
process_options([H|T1], [H|T2]) :-
  process_option(H), !,
  process_options(T1, T2).
process_options([_|T1], T2) :-
  process_options(T1, T2).

process_option(cwd(_)).
process_option(detached(_)).
process_option(env(_)).
process_option(priority(_)).
process_option(process(_)).
process_option(stderr(_)).
process_option(stdin(_)).
process_option(stdout(_)).
process_option(window(_)).
