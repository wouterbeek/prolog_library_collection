:- module(
  os_ext,
  [
    cli_arguments/1,    % -Args
    graphviz_open/4,    % +Method, +In, +Format, +Out
    process_flags/3,    % :Goal_2, +Args, -Flags
    process_open/3,     % +Program, +In, -Out
    process_open/4,     % +Program, +In, +Args, -Out
    process_open/5,     % +Program, +In, +Args, -Out, +Options
    run_jar/3,          % +Jar, +Args, :Goal_1
    run_jar/4,          % +Jar, +Args, :Goal_1, +Options
    run_process/2,      % +Program, +Args
    run_process/3,      % +Program, +Args, +Options
    run_process/4,      % +Program, +Args, :Goal_1, +Options
    set_cli_arguments/1 % +Args
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
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(yall)).

%! cli_arguments(-Args:list(compound)) is det.

:- dynamic
    cli_arguments/1.

% TBD: This throws an initialization error during startup, claiming
%      that atom_phrase/2 is undefined.
:- initialization
   init_cli_arguments.

:- meta_predicate
    process_flags(2, +, -),
    run_jar(+, +, 1),
    run_jar(+, +, 1, +),
    run_process(+, +, 1, +).





%! graphviz_open(+Method:atom, +In:stream, +Format:atom, +Out:stream) is det.
%
% @arg Method The algorithm used by GraphViz for positioning the tree
%             nodes.
%
% @arg Format The file type of the GraphViz output file.
%
% @type_error if Method is not a value of graphviz_method/1.
%
% @type_error if Format is not a value of graphviz_format/1.

graphviz_open(Method, In, Format, Out) :-
  call_must_be(graphviz_method, Method),
  call_must_be(graphviz_format, Format),
  atom_concat('-T', Format, Arg),
  process_open(Method, In, [Arg], Out).

graphviz_format(bmp).
graphviz_format(canon).
graphviz_format(dot).
graphviz_format(gv).
graphviz_format(xdot).
graphviz_format('xdot1.2').
graphviz_format('xdot1.4').
graphviz_format(cgimage).
graphviz_format(cmap).
graphviz_format(eps).
graphviz_format(exr).
graphviz_format(fig).
graphviz_format(gd).
graphviz_format(gd2).
graphviz_format(gif).
graphviz_format(gtk).
graphviz_format(ico).
graphviz_format(imap).
graphviz_format(cmapx).
graphviz_format(imap_np).
graphviz_format(cmapx_np).
graphviz_format(ismap).
graphviz_format(jp2).
graphviz_format(jpg).
graphviz_format(jpeg).
graphviz_format(jpe).
graphviz_format(pct).
graphviz_format(pict).
graphviz_format(pdf).
graphviz_format(pic).
graphviz_format(plain).
graphviz_format('plain-ext').
graphviz_format(png).
graphviz_format(pov).
graphviz_format(ps).
graphviz_format(ps2).
graphviz_format(psd).
graphviz_format(sgi).
graphviz_format(svg).
graphviz_format(svgz).
graphviz_format(tga).
graphviz_format(tif).
graphviz_format(tiff).
graphviz_format(tk).
graphviz_format(vml).
graphviz_format(vmlz).
graphviz_format(vrml).
graphviz_format(wbmp).
graphviz_format(webp).
graphviz_format(xlib).
graphviz_format(x11).

graphviz_method(circo).
graphviz_method(dot).
graphviz_method(fdp).
graphviz_method(neato).
graphviz_method(osage).
graphviz_method(sfdp).
graphviz_method(twopi).



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


process_open(Program, In, Args, Out, Options1) :-
  process_options(Options1, Options2),
  merge_options(
    [stderr(pipe(ProcErr)),stdin(pipe(ProcIn)),stdout(pipe(Out))],
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



%! set_cli_arguments(+Args:list(compound)) is det.

set_cli_arguments(Args) :-
  retractall(cli_arguments(_)),
  assert(cli_arguments(Args)).





% INITIALIZATION %

init_cli_arguments :-
  current_prolog_flag(os_argv, Flags),
  convlist(parse_argument, Flags, Args),
  set_cli_arguments(Args).

parse_argument(Flag, Arg) :-
  atom_phrase(argument(Arg), Flag).

argument(Arg) -->
  "--",
  '...'(Codes),
  "=", !,
  {atom_codes(Key, Codes)},
  rest_as_atom(Value),
  {Arg =.. [Key,Value]}.





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
