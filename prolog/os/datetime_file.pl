:- module(
  datetime_file,
  [
    create_date_directory/1, % -Directory:atom
    create_date_directory/2, % +Spec
                             % -Directory:atom
    create_datetime_file/1, % -File
    create_datetime_file/2, % +Spec:compound
                            % -File:atom
    latest_datetime_file/1, % -File
    latest_datetime_file/2 % +Spec:compound
                           % -File:atom
  ]
).

/** <module> Date-time file support

@author Wouter Beek
@version 2015/07, 2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(string_ext)).





%! create_date_directory(-Directory:atom) is det.
% Wrapper around create_date_directory/2 using `.' as the parent directory.

create_date_directory(Dir):-
  create_date_directory(., Dir).


%! create_date_directory(+Spec:compound, -Directory:atom) is det.
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



%! create_datetime_file(-File:atom) is det.
% Wrapper around datetime_file/2 using `.' as the parent directory.

create_datetime_file(File):-
  create_datetime_file(., File).


%! create_datetime_file(+Spec:compound, -File:atom) is det.

create_datetime_file(Spec, File):-
  create_date_directory(Spec, Dir),
  get_time(TS),
  format_time(string(H), "%H", TS),
  format_time(string(Mi), "%M", TS),
  format_time(string(S), "%S", TS),
  string_list_concat([H,Mi,S], "_", Local),
  directory_file_path(Dir, Local, File).



%! latest_datetime_file(-File:atom) is det.
% Wrapper around latest_datetime_file/2 using `.' as the parent directory.

latest_datetime_file(Latest):-
  latest_datetime_file(., Latest).


%! latest_datetime_file(+Spec:compound, -File:atom) is det.

latest_datetime_file(Spec, Latest):-
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
