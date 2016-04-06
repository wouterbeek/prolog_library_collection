:- module(
  date_time_file,
  [
    create_date_directory/2, % +Spec, -Dir
    create_date_time_file/2, % +Spec, -File
    is_older_file/2,         % +Path1, +Path2
    is_younger_file/2,       % +Path1, +Path2
    latest_date_time_file/2  % +Spec, -File
  ]
).

/** <module> Date-time file support

@author Wouter Beek
@version 2015/07, 2015/10-2015/12, 2016/02-2016/03
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(string_ext)).





%! create_date_directory(+Spec:compound, -Directory) is det.
% Create and return the current date subdirectory of the given absolute
% directory name.
%
% Example: `/tmp' ⇒ `/tmp/2013/05/10'

create_date_directory(Spec, Dir):-
  get_time(TS),
  format_time(atom(D), "%d", TS),
  format_time(atom(Mo), "%m", TS),
  format_time(atom(Y), "%Y", TS),
  absolute_file_name(Spec, PrefixDir, [access(write),file_type(directory)]),
  string_list_concat([Y,Mo,D], "/", PostfixDir),
  directory_file_path(PrefixDir, PostfixDir, Dir),
  make_directory_path(Dir).



%! create_date_time_file(+Spec:compound, -File) is det.

create_date_time_file(Spec, File):-
  create_date_directory(Spec, Dir),
  get_time(TS),
  format_time(string(H), "%H", TS),
  format_time(string(Mi), "%M", TS),
  format_time(string(S), "%S", TS),
  string_list_concat([H,Mi,S], "_", Local),
  directory_file_path(Dir, Local, File).



%! is_older_file(+Path1, +Path2) is semidet.

is_older_file(Path1, Path2):-
  is_younger_file(Path2, Path1).



%! is_younger_file(+Path1, +Path2) is semidet.
% Succeeds if the file denoted by Path1 is younger than
% the file denoted by Path2.

is_younger_file(Path1, Path2):-
  time_file(Path1, T1),
  time_file(Path2, T2),
  T1 > T2.



%! latest_date_time_file(+Spec, -File) is det.

latest_date_time_file(Spec, Latest):-
  absolute_file_name(Spec, Dir, [access(read),file_type(directory)]),

  % Year
  directory_files(Dir, Ys0),
  include(atom_phrase(integer(_)), Ys0, Ys),
  max_member(Y, Ys),
  directory_file_path(Dir, Y, YDir),

  % Month
  directory_files(YDir, Mos0),
  include(atom_phrase(integer(_)), Mos0, Mos),
  max_member(Mo, Mos),
  directory_file_path(YDir, Mo, MoDir),

  % Day
  directory_files(MoDir, Ds0),
  include(atom_phrase(integer(_)), Ds0, Ds),
  max_member(D, Ds),
  directory_file_path(MoDir, D, DDir),

  % Time
  directory_files(DDir, DFiles0),
  include(atom_phrase(date_or_time), DFiles0, DFiles),
  max_member(LatestLocal, DFiles),
  
  directory_file_path(DDir, LatestLocal, Latest).

date_or_time -->
  integer(_),
  "_",
  integer(_),
  "_",
  integer(_),
  ... .
