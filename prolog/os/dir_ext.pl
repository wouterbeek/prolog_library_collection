:- module(
  dir_ext,
  [
    append_dirs/2,        % +Dirs, -Dir
    append_dirs/3,        % +Dir1, +Dir2, -Dir
    current_dir/1,        % ?Dir
    dir_file/2,           % +Dir, -File
    dir_file_recursive/2, % +Dir, -File
    dir_subdirs/2,        % ?Dir, ?Subdirs
    run_in_dir/2,         % :Goal_0, +Dir
    wd/1                  % ?Dir
  ]
).
:- reexport(library(filesex)).

/** <module> Directory extensions

Extensions for handling directory files in SWI-Prolog.

@author Wouter Beek
@version 2015/08-2015/09, 2015/11, 2016/01, 2016/03, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(yall)).

:- meta_predicate
    run_in_dir(0, +).





%! append_dirs(+Dirs, -Dir) is det.

append_dirs([], '/') :- !.
append_dirs([Dir], Dir) :- !.
append_dirs([H|T], Dir) :-
  append_dirs0(H, T, Dir).

append_dirs0(Dir, [], Dir) :- !.
append_dirs0(Dir1, [H|T], Dir) :-
  append_dirs(Dir1, H, Dir2),
  append_dirs0(Dir2, T, Dir).


%! append_dirs(+Dir1, +Dir2, -Dir) is det.
% Returns the directory name obtained by concatenating
% the given directory names.
%
% The empty atom in the first position indicates the root directory.
%
% Does *not* ensure that any of the directories exist.

append_dirs(Dir1, Dir2, Dir3) :-
  dir_subdirs(Dir1, Subdirs1),
  dir_subdirs(Dir2, Subdirs2),
  append(Subdirs1, Subdirs2, Subdirs3),
  dir_subdirs(Dir3, Subdirs3).



%! current_dir(+Dir) is semidet.
%! current_dir(-Dir) is det.

current_dir(Dir) :-
  absolute_file_name(., Dir, [file_type(directory)]).



% dir_file(+Dir, -File) is nondet.

dir_file(Dir, File) :-
  directory_files(Dir, Files0),
  member(File0, Files0),
  \+ is_dummy_file(File0),
  directory_file_path(Dir, File0, File).



%! dir_file_recursive(+Dir, -File) is nondet.

dir_file_recursive(Dir, File) :-
  dir_file(Dir, File0),
  (   exists_directory(File0)
  ->  dir_file_recursive(File0, File)
  ;   File = File0
  ).



%! dir_subdirs(+Dir, -Subdirs) is det.
%! dir_subdirs(-Dir, +Subdirs) is det.
% Occurrences of `.` and `..` in Dir are resolved.
%
% The empty atom in the first position indicates the root directory.
%
% For absolute directory names the first subdirectory name is the empty atom.

dir_subdirs(Dir, Subdirs2) :-
  ground(Dir), !,
  atomic_list_concat(Subdirs1, /, Dir),
  resolve_subdirs(Subdirs1, Subdirs2).
dir_subdirs(Dir, Subdirs1) :-
  resolve_subdirs(Subdirs1, Subdirs2),
  atomic_list_concat(Subdirs2, /, Dir).



%! run_in_dir(:Goal_0, +Dir) is det.

run_in_dir(Goal_0, Dir) :-
  working_directory(Dir0, Dir),
  Goal_0,
  working_directory(Dir, Dir0).



%! wd(+Dir) is semidet.
%! wd(-Dir) is det.

wd(Dir) :-
  working_directory(Dir, Dir).





% HELPERS %

%! is_dummy_file(+File) is semidet.

is_dummy_file(.).
is_dummy_file(..).



%! resolve_subdirs(+Subdirs, -ResoledSubdirs) is det.

resolve_subdirs([], []) :- !.
resolve_subdirs([''], []) :- !.
resolve_subdirs([.|T1], T2) :- !,
  resolve_subdirs(T1, T2).
resolve_subdirs([_,..|T1], T2) :- !,
  resolve_subdirs(T1, T2).
resolve_subdirs([H|T1], [H|T2]) :-
  resolve_subdirs(T1, T2).
