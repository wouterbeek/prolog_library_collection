:- module(
  os_ext,
  [
    cli_arguments/1, % -Arguments
    process_flags/3, % :Goal_2, +Arguments, -Flags
    process_open/3,  % +Program, +In1, -In2
    process_open/4,  % +Program, +In1, +Arguments, -In2
    process_open/5,  % +Program, +In1, +Arguments, -In2, +Options
    run_jar/3,       % +Jar, +Arguments, :Goal_1
    run_jar/4,       % +Jar, +Arguments, :Goal_1, +Options
    run_process/2,   % +Program, +Arguments
    run_process/3,   % +Program, +Arguments, +Options
    run_process/4    % +Program, +Arguments, :Goal_1, +Options
  ]
).

/** <module> OS extensions

@author Wouter Beek
@version 2017/04-2017/07
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(yall)).

%! cli_arguments(-Arguments:list(compound)) is det.

:- dynamic
    cli_arguments/1.

% TBD: This throws an initialization error during startup, claiming
%      that atom_phrase/2 is undefined.
%:- initialization
%   init_cli_arguments.

:- meta_predicate
    process_flags(2, +, -),
    run_jar(+, +, 1),
    run_jar(+, +, 1, +),
    run_process(+, +, 1, +).





%! process_flags(:Goal_2, +Arguments:list(compound), -Flags:list(atom)) is det.

process_flags(_, [], []).
process_flags(Goal_2, [H1|T1], [H2|T2]) :-
  call(Goal_2, H1, H2), !,
  process_flags(Goal_2, T1, T2).
process_flags(Goal_2, [_|T], L) :-
  process_flags(Goal_2, T, L).



%! process_open(+Program:atom, +In1:stream, -In2:stream) is det.
%! process_open(+Program:atom, +In1:stream, +Arguments:list(term),
%!              -In2:stream) is det.
%! process_open(+Program:atom, +In1:stream, +Arguments:list(term),
%!              -In2:stream, +Options:list(compound)) is det.
%
% In1 is closed by this predicate.

process_open(Program, In1, In2) :-
  process_open(Program, In1, [], In2).


process_open(Program, In1, Arguments, In2) :-
  process_open(Program, In1, Arguments, In2, []).


process_open(Program, In1, Arguments, In2, Options1) :-
  process_options(Options1, Options2),
  merge_options(
    [stderr(pipe(ProcErr)),stdin(pipe(ProcIn)),stdout(pipe(In2))],
    Options2,
    Options3
  ),
  process_debug(Program, Arguments),
  process_create(path(Program), Arguments, Options3),
  set_stream(ProcIn, type(binary)),
  thread_create(copy_and_close(In1, ProcIn), _, [detached(true)]),
  thread_create(print_error(ProcErr), _, [detached(true)]).

copy_and_close(In, Out) :-
  call_cleanup(
    copy_stream_data(In, Out),
    close(Out)
  ).



%! run_jar(+Jar:atom, +Arguments:list(term), :Goal_1) is det.
%! run_jar(+Jar:atom, +Arguments:list(term), :Goal_1,
%!         +Options:list(compound)) is det.

run_jar(Jar, Arguments, Goal_1) :-
  run_jar(Jar, Arguments, Goal_1, []).


run_jar(Jar, Arguments, Goal_1, Options) :-
  run_process(java, ['-jar',file(Jar)|Arguments], Goal_1, Options).



%! run_process(+Program:atom, +Arguments:list(term)) is det.
%! run_process(+Program:atom, +Arguments:list(term), :Goal_1) is det.
%! run_process(+Program:atom, +Arguments:list(term), :Goal_1,
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

run_process(Program, Arguments) :-
  run_process(Program, Arguments, []).


run_process(Program, Arguments, Options) :-
  run_process(
    Program,
    Arguments,
    [Out]>>copy_stream_data(Out, user_output),
    Options
  ).


run_process(Program, Arguments, Goal_1, Options1) :-
  process_options(Options1, Options2),
  merge_options(
    Options2,
    [process(Pid),stderr(pipe(Err)),stdout(pipe(Out))],
    Options3
  ),
  % `Program' is either an absolute file or a related file that is
  % resolved WRT the PATH environment variable.
  (is_absolute_file_name(Program) -> Exec = Program ; Exec = path(Program)),
  process_debug(Program, Arguments),
  setup_call_cleanup(
    process_create(Exec, Arguments, Options3),
    (
      call(Goal_1, Out),
      thread_create(print_error(Err), _, [detached(true)]),
      process_wait(Pid, exit(OutStatus)),
      process_status(OutStatus)
    ),
    close(Out)
  ).

process_debug(Program, Arguments) :-
  debugging(process_open), !,
  Goal_0 =.. [Program|Arguments],
  debug(process_open, "~w", [Goal_0]).
process_debug(_, _).

process_status(0) :- !.
process_status(exit(Status)) :-
  print_message(warning, process_status(Status)).





% INITIALIZATION %

init_cli_arguments :-
  current_prolog_flag(os_argv, Flags),
  convlist(parse_argument, Flags, Arguments),
  assert(cli_arguments(Arguments)).

parse_argument(Flag, Argument) :-
  atom_phrase(argument(Argument), Flag).

argument(Argument) -->
  "--",
  '...'(Codes),
  "=", !,
  {atom_codes(Key, Codes)},
  rest_as_atom(Value),
  {Argument =.. [Key,Value]}.





% HELPERS %

%! print_error(+Error:stream) is det.

print_error(Err) :-
  call_cleanup(
    copy_stream_data(Err, user_error),
    close(Err)
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
