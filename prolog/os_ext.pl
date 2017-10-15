:- module(
  os_ext,
  [
    convert_to/2,     % +Format, +File
    exists_program/1, % +Program
    open_pdf/1,       % +File
    os/1,             % ?Os
    os_path/1,        % ?Directory
    process_flags/3,  % :Goal_2, +Args, -Flags
    process_status/1  % +Status
  ]
).

/** <module> OS extensions

@author Wouter Beek
@version 2017/04-2017/10
*/

:- use_module(library(call_ext)).
:- use_module(library(debug)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(stream_ext)).
:- use_module(library(yall)).

:- meta_predicate
    process_flags(2, +, -).





%! convert_to(+Format:atom, +File:atom) is det.
%
% @see Formats are ‘documented’ over at
% https://cgit.freedesktop.org/libreoffice/core/tree/filter/source/config/fragments/filters

convert_to(Format, File) :-
  call_must_be(convert_format, Format),
  setup_call_cleanup(
    process_create(
      path(libreoffice),
      ['--convert-to',Format,file(File)],
      [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
    ),
    (
      thread_create(copy_stream_data(ProcOut, user_output), _, [detached(true)]),
      thread_create(print_err(ProcErr), _, [detached(true)]),
      process_wait(Pid, exit(Status)),
      process_status(Status)
    ),
    close(ProcOut)
  ).

convert_format(csv).



%! exists_program(+Program:atom) is semidet.
%
% Succeeds if the given program can be run from PATH.

exists_program(Program) :-
  os_path(Prefix),
  atomic_list_concat([Prefix,Program], /, Exe),
  access_file(Exe, execute), !.



%! open_pdf(+File:atom) is det.
%
% Opens the given PDF file.

open_pdf(File) :-
  once((
    member(Program, [evince,xpdf]),
    exists_program(Program)
  )),
  process_create(
    path(Program),
    [file(File)],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  thread_create(copy_data_stream(ProcErr, user_error), _, [detached(true)]),
  thread_create(copy_data_stream(ProcOut, user_output), _, [detached(true)]),
  process_wait(Pid, exit(Status)),
  process_status(Status).



%! os(+Os:oneof([mac,unix,windows])) is semidet.
%! os(-Os:oneof([mac,unix,windows])) is det.
%
% Succeeds if Os denotes the current Operating System.

os(mac) :-
  current_prolog_flag(apple, true), !.
os(unix) :-
  current_prolog_flag(unix, true), !.
os(windows) :-
  current_prolog_flag(windows, true), !.



%! os_path(+Directory:atom) is semidet.
%! os_path(-Directory:atom) is nondet.
%
% Succeeds if Directory is on the OS PATH.

os_path(OsDir) :-
  getenv('PATH', Path),
  os_path_separator(Sep),
  atomic_list_concat(Dirs, Sep, Path),
  member(Dir, Dirs),
  prolog_to_os_filename(OsDir, Dir).



%! os_path_separator(-Separator:oneof([:,;])) is det.

os_path_separator(Sep) :-
  os(Os),
  os_path_separator(Os, Sep).

os_path_separator(mac, :).
os_path_separator(unix, :).
os_path_separator(windows, ;).



%! process_flags(:Goal_2, +Args:list(compound), -Flags:list(atom)) is det.

process_flags(_, [], []).
process_flags(Goal_2, [H1|T1], [H2|T2]) :-
  call(Goal_2, H1, H2), !,
  process_flags(Goal_2, T1, T2).
process_flags(Goal_2, [_|T], L) :-
  process_flags(Goal_2, T, L).



%! process_status(+Status:nonneg) is det.

process_status(0) :- !.
process_status(Status) :-
  print_message(warning, process_status(Status)).
