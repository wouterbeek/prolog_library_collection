:- module(
  os_ext,
  [
    process/2,        % +Program, +Arguments
    process_in/2,     % +Program, :In_1
    process_in_out/3, % +Program, :In_1, :Out_1
    process_in_out/4, % +Program, +Arguments, :In_1, :Out_1
    process_in_out/5, % +Program, +Arguments, :In_1, :Out_1, +Options
    exists_program/1, % +Program
    open_file/1,      % +File
    open_file/2,      % +MediaType, +File
    os/1,             % ?Os
    os_path/1         % ?Directory
  ]
).

/** <module> OS extensions

@author Wouter Beek
@version 2017/04-2018/01
*/

:- use_module(library(media_type)).
:- use_module(library(process)).
:- use_module(library(thread_ext)).
:- use_module(library(yall)).

:- meta_predicate
    process_in(+, 1),
    process_in_out(+, 1, 1),
    process_in_out(+, +, 1, 1),
    process_in_out(+, +, 1, 1, +),
    process_in_out_err(+, +, 1, 1, 1, +),
    process_out_err(+, +, 1, 1, +).





%! process(+Program:atom, +Arguments:list(compound)) is det.

process(Program, Args) :-
  process_out_err(
    Program,
    Args,
    [ProcOut]>>copy_stream_data(ProcOut, user_output),
    [ProcErr]>>copy_stream_data(ProcErr, user_error),
    []
  ).



%! process_in(+Program:atom, :In_1) is det.

process_in(Program, In_1) :-
  process_in_out(Program, In_1, [ProcOut]>>copy_stream_data(ProcOut, user_output)).



%! process_in_out(+Program:atom, :In_1, :Out_1) is det.
%! process_in_out(+Program:atom, +Arguments:list(compound), :In_1, :Out_1) is det.
%! process_in_out(+Program:atom, +Arguments:list(compound), :In_1, :Out_1,
%!                +Options:list(compound)) is det.

process_in_out(Program, In_1, Out_1) :-
  process_in_out(Program, [], In_1, Out_1).


process_in_out(Program, Args, In_1, Out_1) :-
  process_in_out(Program, Args, In_1, Out_1, []).


process_in_out(Program, Args, In_1, Out_1, Options) :-
  process_in_out_err(
    Program,
    Args,
    In_1,
    Out_1,
    [ProcErr]>>copy_stream_data(ProcErr, user_error),
    Options
  ).



%! process_in_out_err(+Program:atom, +Arguments:list(compound), :In_1, :Out_1,
%!                    :Err_1, +Options:list(compound)) is det.

process_in_out_err(Program, Args, In_1, Out_1, Err_1, Options) :-
  setup_call_cleanup(
    process_create(
      path(Program),
      Args,
      [
        process(Pid),
        stderr(pipe(ProcErr)),
        stdin(pipe(ProcIn)),
        stdout(pipe(ProcOut))
      | Options
      ]
    ),
    (
      thread_create(call(Err_1, ProcErr), ErrId),
      thread_create(call(Out_1, ProcOut), OutId),
      call_cleanup(
        call(In_1, ProcIn),
        close(ProcIn)
      ),
      process_wait(Pid, exit(Status)),
      (Status =:= 0 -> true ; throw(error(process_status(Status))))
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




%! process_out_err(+Program:atom, +Arguments:list(compound), :Out_1, :Err_1,
%!                 +Options:list(compound)) is det.

process_out_err(Program, Args, Out_1, Err_1, Options) :-
  setup_call_cleanup(
    process_create(
      path(Program),
      Args,
      [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))|Options]
    ),
    (
      thread_create(call(Err_1, ProcErr), ErrId),
      thread_create(call(Out_1, ProcOut), OutId),
      process_wait(Pid, exit(Status)),
      (Status =:= 0 -> true ; throw(error(process_status(Status))))
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
