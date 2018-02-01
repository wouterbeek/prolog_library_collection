:- module(
  os_ext,
  [
    process/1,         % +Program
    process/2,         % +Program, +Arguments
    process/3,         % +Program, +Arguments, :Out_1
    process/4,         % +Program, +Arguments, :Out_1, :Err_1
    process/5,         % +Program, +Arguments, :Out_1, :Err_1, +Options
    process_in/2,      % +Program, +In
    process_in/3,      % +Program, +Arguments, +In
    process_in/4,      % +Program, +Arguments, +In, :Out_1
    process_in/5,      % +Program, +Arguments, +In, :Out_1, :Err_1
    process_in/6,      % +Program, +Arguments, +In, :Out_1, :Err_1, +Options
    process_in_open/3, % +Program, +In, -Out
    process_in_open/4, % +Program, +Arguments, +In, -Out
    process_in_open/5, % +Program, +Arguments, +In, -Out, +Options
    exists_program/1,  % +Program
    open_file/1,       % +File
    open_file/2,       % +MediaType, +File
    os/1,              % ?Os
    os_path/1          % ?Directory
  ]
).

/** <module> OS extensions

@author Wouter Beek
@version 2017/04-2018/01
*/

:- use_module(library(media_type)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(thread_ext)).
:- use_module(library(yall)).

:- meta_predicate
    process(+, +, 1),
    process(+, +, 1, 1),
    process(+, +, 1, 1, +),
    process_(1, +, +, -),
    process_in(+, +, +, 1),
    process_in(+, +, +, 1, 1),
    process_in(+, +, +, 1, 1, +).





%! process(+Program:atom) is det.
%! process(+Program:atom, +Arguments:list) is det.
%! process(+Program:atom, +Arguments:list, :Out_1) is det.
%! process(+Program:atom, +Arguments:list, :Out_1, :Err_1) is det.
%! process(+Program:atom, +Arguments:list, :Out_1, :Err_1, +Options:list(compound)) is det.

process(Program) :-
  process(Program, []).


process(Program, Args) :-
  process(Program, Args, _).


process(Program, Args, Out_1) :-
  process(Program, Args, Out_1, _).


process(Program, Args, Out_1, Err_1) :-
  process(Program, Args, Out_1, Err_1, []).


process(Program, Args, Out_1, Err_1, Options0) :-
  merge_options(
    [stderr(pipe(ProcErr)),stdout(pipe(ProcOut))],
    Options0,
    Options
  ),
  setup_call_cleanup(
    process_create(path(Program), Args, Options),
    (
      process_(Err_1, ProcErr, user_error, ErrId),
      process_(Out_1, ProcOut, user_output, OutId)
    ),
    call_cleanup(
      thread_join(ErrId),
      call_cleanup(
        thread_join(OutId),
        call_cleanup(
          close(ProcErr),
          close(ProcOut)
        )
      )
    )
  ).

process_(Goal_1, Proc, Stream, Id) :-
  strip_module(Goal_1, _, Goal),
  var(Goal), !,
  thread_create(copy_stream_data(Proc, Stream), Id).
process_(Goal_1, Proc, _, Id) :-
  thread_create(call(Goal_1, Proc), Id).



%! process_in(+Program:atom, +In:stream) is det.
%! process_in(+Program:atom, +Arguments:list, +In:stream) is det.
%! process_in(+Program:atom, +Arguments:list, +In:stream, :Out_1) is det.
%! process_in(+Program:atom, +Arguments:list, +In:stream, :Out_1, :Err_1) is det.
%! process_in(+Program:atom, +Arguments:list, +In:stream, :Out_1, :Err_1, +Options:list(compound)) is det.

process_in(Program, In) :-
  process_in(Program, [], In).


process_in(Program, Args, In) :-
  process_in(Program, Args, In, _).


process_in(Program, Args, In, Out_1) :-
  process_in(Program, Args, In, Out_1, _).


process_in(Program, Args, In, Out_1, Err_1) :-
  process_in(Program, Args, In, Out_1, Err_1, []).


process_in(Program, Args, In, Out_1, Err_1, Options0) :-
  merge_options(
    [stderr(pipe(ProcErr)),stdin(pipe(ProcIn)),stdout(pipe(ProcOut))],
    Options0,
    Options
  ),
  setup_call_cleanup(
    process_create(path(Program), Args, Options),
    (
      process_(Err_1, ProcErr, user_error, ErrId),
      process_(Out_1, ProcOut, user_output, OutId),
      call_cleanup(
        copy_stream_data(In, ProcIn),
        close(ProcIn)
      )
    ),
    call_cleanup(
      thread_join(ErrId),
      call_cleanup(
        thread_join(OutId),
        call_cleanup(
          close(ProcErr),
          close(ProcOut)
        )
      )
    )
  ).



%! process_in_open(+Program:atom, +In:stream, -Out:stream) is det.
%! process_in_open(+Program:atom, +Arguments:list, +In:stream, -Out:stream) is det.
%! process_in_open(+Program:atom, +Arguments:list, +In:stream, -Out:stream, +Options:list(compound)) is det.

process_in_open(Program, In, Out) :-
  process_in_open(Program, [], In, Out).


process_in_open(Program, Args, In, Out) :-
  process_in_open(Program, Args, In, Out, []).


process_in_open(Program, Args, In, Out, Options0) :-
  merge_options(
    [stderr(pipe(ProcErr)),stdin(pipe(ProcIn)),stdout(pipe(Out))],
    Options0,
    Options
  ),
  process_create(path(Program), Args, Options),
  thread_create(handle_err(ProcErr), _, [detached(true)]),
  thread_create(
    call_cleanup(
      copy_stream_data(In, ProcIn),
      close(ProcIn)
    ),
    _,
    [detached(true)]
  ).

handle_err(Err) :-
  at_end_of_stream(Err), !,
  close(Err).
handle_err(Err) :-
  copy_stream_data(Err, user_error),
  handle_err(Err).



%! exists_program(+Program:atom) is semidet.
%
% Succeeds if the given program can be run from PATH.

exists_program(Program) :-
  os_path(Prefix),
  atomic_list_concat([Prefix,Program], /, Exe),
  access_file(Exe, execute), !.



%! open_file(+File:atom) is semidet.
%! open_file(+MediaType:compound, +File:atom) is det.
%
% Open the file using the first existing program that is registered
% with the Media Type denote by its file name extension.
%
% Fails if there is no file name extension, or if the file name
% extension cannot be mapped to a Media Type, or if the Media Type
% cannot be mapped to a program, or if none of the mapped to programs
% exists.

open_file(File) :-
  file_name_extension(_, Ext, File),
  media_type_extension(MediaType, Ext),
  open_file(MediaType, File).


open_file(MediaType, File) :-
  media_type_program(MediaType, Program, Args),
  process(Program, [file(File)|Args]).



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
