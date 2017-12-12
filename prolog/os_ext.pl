:- module(
  os_ext,
  [
    exists_program/1, % +Program
    open_file/1,      % +File
    open_file/2,      % +MediaType, +File
    os/1,             % ?Os
    os_path/1,        % ?Directory
    process_flags/3   % :Goal_2, +Args, -Flags
  ]
).

/** <module> OS extensions

@author Wouter Beek
@version 2017/04-2017/12
*/

:- use_module(library(media_type)).
:- use_module(library(process)).

:- meta_predicate
    process_flags(2, +, -).





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
  exists_program(Program), !,
  process_create(
    path(Program),
    [file(File)|Args],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  thread_create(copy_stream_data(ProcErr, user_error), _, [detached(true)]),
  thread_create(copy_stream_data(ProcOut, user_output), _, [detached(true)]),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))).



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
