:- module(
  directory_ext,
  [
    append_directories/2,       % +Dirs, -Dir
    append_directories/3,       % +Dir1, +Dir2, -Dir
    current_directory/1,        % ?Dir
    directory_file/2,           % +Dir, -File
    directory_file_recursive/2, % +Dir, -File
    directory_path/2,           % +Dir, -Path
    directory_subdirectories/2, % ?Dir, ?Subdirs
    run_in_directory/2,         % :Goal_0, +Dir
    wd/1                        % ?Dir
  ]
).
:- reexport(library(filesex)).

/** <module> Directory extensions

Extensions for handling directory files in SWI-Prolog.

@author Wouter Beek
@version 2015/08-2015/09, 2015/11, 2016/01, 2016/03, 2016/05, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(yall)).

:- meta_predicate
    run_in_directory(0, +).





%! append_directories(+Dirs, -Dir) is det.

append_directories([], '/') :- !.
append_directories([Dir], Dir) :- !.
append_directories([H|T], Dir) :-
  append_directories0(H, T, Dir).

append_directories0(Dir, [], Dir) :- !.
append_directories0(Dir1, [H|T], Dir) :-
  append_directories(Dir1, H, Dir2),
  append_directories0(Dir2, T, Dir).


%! append_directories(+Dir1, +Dir2, -Dir) is det.
%
% Returns the directory name obtained by concatenating the given
% directory names.
%
% The empty atom in the first position indicates the root directory.
%
% Does *not* ensure that any of the directories exist.

append_directories(Dir1, Dir2, Dir3) :-
  directory_subdirectories(Dir1, Subdirs1),
  directory_subdirectories(Dir2, Subdirs2),
  append(Subdirs1, Subdirs2, Subdirs3),
  directory_subdirectories(Dir3, Subdirs3).



%! current_directory(+Dir) is semidet.
%! current_directory(-Dir) is det.

current_directory(Dir) :-
  absolute_file_name(., Dir, [file_type(directory)]).



% directory_file(+Dir, -File) is nondet.
%
% Non-deterministic variant of directory_files/2 that skips dummy
% files.

directory_file(Dir, File) :-
  directory_files(Dir, Files),
  member(File, Files),
  \+ is_dummy_file(File).



%! directory_file_recursive(+Dir, -File) is nondet.

directory_file_recursive(Dir, File) :-
  directory_file(Dir, File0),
  (   exists_directory(File0)
  ->  directory_file_recursive(File0, File)
  ;   File = File0
  ).



%! directory_path(+Dir, -Path) is nondet.

directory_path(Dir, Path) :-
  directory_file(Dir, File),
  directory_file_path(Dir, File, Path).



%! directory_subdirectories(+Dir, -Subdirs) is det.
%! directory_subdirectories(-Dir, +Subdirs) is det.
%
% Occurrences of `.` and `..` in Dir are resolved.
%
% The empty atom in the first position indicates the root directory.
%
% For absolute directory names the first subdirectory name is the
% empty atom.

directory_subdirectories(Dir, Subdirs2) :-
  ground(Dir), !,
  atomic_list_concat(Subdirs1, /, Dir),
  resolve_subdirectories(Subdirs1, Subdirs2).
directory_subdirectories(Dir, Subdirs1) :-
  resolve_subdirectories(Subdirs1, Subdirs2),
  atomic_list_concat(Subdirs2, /, Dir).



%! is_dummy_file(+File) is semidet.

is_dummy_file(.).
is_dummy_file(..).



%! run_in_directory(:Goal_0, +Dir) is det.

run_in_directory(Goal_0, Dir) :-
  working_directory(Dir0, Dir),
  Goal_0,
  working_directory(Dir, Dir0).



%! wd(+Dir) is semidet.
%! wd(-Dir) is det.

wd(Dir) :-
  working_directory(Dir, Dir).





% HELPERS %

%! resolve_subdirectories(+Subdirs, -ResoledSubdirs) is det.

resolve_subdirectories([], []) :- !.
resolve_subdirectories([''], []) :- !.
resolve_subdirectories([.|T1], T2) :- !,
  resolve_subdirectories(T1, T2).
resolve_subdirectories([_,..|T1], T2) :- !,
  resolve_subdirectories(T1, T2).
resolve_subdirectories([H|T1], [H|T2]) :-
  resolve_subdirectories(T1, T2).
