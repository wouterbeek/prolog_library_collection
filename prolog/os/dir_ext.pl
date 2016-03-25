:- module(
  dir_ext,
  [
    append_dirs/2,   % +Dirs, -Dir
    append_dirs/3,   % +Dir1, +Dir2, -Dir
    current_dir/1,   % ?Dir
    dir_subdirs/2,   % ?Dir, ?Subdirs
    direct_subdir/2, % +Dir, -Subdir
    run_in_dir/2,    % :Goal_0, +Dir
    wd/1             % ?Dir
  ]
).
:- reexport(library(filesex)).

/** <module> Directory extensions

Extensions for handling directory files in SWI-Prolog.

@author Wouter Beek
@version 2015/08-2015/09, 2015/11, 2016/01, 2016/03
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



%! dir_subdirs(+Dir, -Subdirs) is det.
%! dir_subdirs(-Dir, +Subdirs) is det.
% Occurrences of `.` and `..` in Dir are resolved.
%
% For absolute directory names the first subdirectory name is the empty atom.

dir_subdirs(Dir, Subdirs2) :-
  ground(Dir), !,
  atomic_list_concat(Subdirs1, /, Dir),
  resolve_subdirs(Subdirs1, Subdirs2).
dir_subdirs(Dir, Subdirs1) :-
  resolve_subdirs(Subdirs1, Subdirs2),
  atomic_list_concat(Subdirs2, /, Dir).



% direct_subdir(+Dir, -Subdir) is nondet.

direct_subdir(Dir1, Dir2) :-
  directory_files(Dir1, Subdirs),
  member(Subdir, Subdirs),
  \+ is_dummy_file(Subdir),
  directory_file_path(Dir1, Subdir, Dir2).



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
