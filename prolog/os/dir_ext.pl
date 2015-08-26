:- module(
  dir_ext,
  [
    append_directories/3, % +Dir1:atom
                          % +Dir2:atom
                          % -Dir3:atom
    create_directory/1, % +Directory:atom
    create_directory/2, % +Current:compound
                        % +Subdirs:list(atom)
    create_directory/3, % +Current:compound
                        % +Subdirs:list(atom)
                        % -Dir:atom
    current_directory/1, % ?Directory:atom
    date_directory/2, % +Spec
                      % -Directory:atom
    directory_files/3, % +Directory:atom
                       % -AbsoluteFiles:list(atom)
                       % +Options:list(compound)
    directory_subdirectories/2 % ?Directory:atom
                               % ?Subdirectories:list(atom)
  ]
).
:- reexport(library(filesex)).

/** <module> Directory extensions

Extensions for handling directory files in SWI-Prolog.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(option)).

:- predicate_options(directory_files/3, 3, [
     file_types(+list(atom)),
     include_directories(+boolean),
     include_self(+boolean),
     recursive(+boolean)
   ]).





%! append_directories(+Dir1:atom, +Dir2:atom, -Dir3:atom) is det.
% Returns the directory name obtained by concatenating
% the given directory names.
%
% Does *not* ensure that any of the directories exist.

append_directories(Dir1, Dir2, Dir3):-
  directory_subdirectories(Dir1, Subdirs1),
  directory_subdirectories(Dir2, Subdirs2),
  append(Subdirs1, Subdirs2, Subdirs3),
  directory_subdirectories(Dir3, Subdirs3).



%! create_directory(+Abs:atom) is det.
% Creates a directory with the given absolute file path.
%
% This predicate does not support argument `Spec` argument
% since absolute_file_name/3 does not work
% with path names that denote non-existing directories.
% @see http://www.swi-prolog.org/pldoc/doc_for?object=absolute_file_name/3

% Current directory: nothing to create.
create_directory(.):- !.
% Not an absolute path.
create_directory(Abs):-
  \+ is_absolute_file_name(Abs), !,
  domain_error(absolute_path, Abs).
% Directory already exists.
create_directory(Abs):-
  exists_directory(Abs), !.
% Create directory.
create_directory(Abs):-
  directory_subdirectories(Abs, Subdirs),
  % Recursively assert all subpaths.
  % The root node is indicated by the empty atom.
  create_directory0('', Subdirs, _).

%! create_directory(+Current:compound, +Subdirs:list(atom)) is det.

create_directory(Current, Subdirs):-
  create_directory(Current, Subdirs, _).

%! create_directory(
%!   +Current:compound,
%!   +Subdirs:list(atom),
%!   -AbsoluteDir:atom
%! ) is det.
% Creates a nested directory based on:
%   - a current directory
%   - a list of consecutive subdirectories

create_directory(Current, Subdirs, Abs):-
  absolute_file_name(Current, Dir0, [access(write),file_type(directory)]),
  create_directory0(Dir0, Subdirs, Abs).

create_directory0(Abs, [], Abs):- !.
create_directory0(Current, [Sub|Subs], Abs):-
  directory_file_path(Current, Sub, Dir),
  (exists_directory(Dir) -> true ; make_directory(Dir)),
  create_directory0(Dir, Subs, Abs).



%! current_directory(+Directory:atom) is semidet.
%! current_directory(-Directory:atom) is det.

current_directory(Dir):-
  absolute_file_name(., Dir, [file_type(directory)]).



%! date_directory(+Spec:compound, -Directory:atom) is det.
% Create and return the current date subdirectory of the given absolute
% directory name.
%
% Example: from `/tmp` to `/tmp/2013/05/10`

date_directory(Spec, Dir):-
  get_time(TimeStamp),
  format_time(atom(Day), '%d', TimeStamp),
  format_time(atom(Month), '%m', TimeStamp),
  format_time(atom(Year), '%Y', TimeStamp),
  absolute_file_name(Spec, PrefixDir, [access(write),file_type(directory)]),
  directory_subdirectories(PostfixDir, [Year,Month,Day]),
  append_directories(PrefixDir, PostfixDir, Dir),
  create_directory(Dir).



%! directory_files(
%!   +Directory:atom,
%!   -AbsoluteFiles:list(atom),
%!   +Options:list(compound)
%! ) is det.
% Variant of directory_files/2 that returns absolute file names
% instead of relative ones and excludes non-file entries.
%
% The following options are supported:
%   * file_types(+FileTypes:list(atom))
%     A list of atomic file types that are used as a filter.
%     Default is no file type filter.
%   * include_directories(+IncludeDirectories:boolean)
%     Whether (sub)directories are included or not.
%     Default is `false`.
%   * include_self(+IncludeSelf:boolean)
%     Whether or not the enclosing directory is included.
%     Default is `false`.
%   * recursive(+Recursive:boolean)
%     Whether subdirectories are searched recursively.
%     Default is `true`.

directory_files(Dir, Files3, Opts1):-
  % Note that the list of files is *not* ordered!
  directory_files(Dir, New1),

  % Remove `.` and `..`.
  exclude(nonfile_entry, New1, New2),

  % Make the file names absolute.
  maplist(directory_file_path(Dir), New2, New3),

  partition(exists_directory, New3, NewDirs, NewFiles1),

  % Filter based on a list of file types, if given.
  (   option(file_types(FileTypes), Opts1)
  ->  include(
        \File^(
          file_name_extension(_, Ext, File),
          user:prolog_file_type(Ext, FileType),
          memberchk(FileType, FileTypes)
        ),
        NewFiles1,
        NewFiles2
      )
  ;   NewFiles2 = NewFiles1
  ),

  % Make sure the `include_self` option is excluded from
  % the processing of subdirectories.
  select_option(include_self(IncludeSelf), Opts1, Opts2, false),

  % Include directories and files from deeper recursion levels.
  (   option(recursive(true), Opts2)
  ->  maplist(
        \NewDir^NewFiles^directory_files(NewDir, NewFiles, Opts2),
        NewDirs,
        NewFiless
      ),
      append([NewFiles2|NewFiless], Files1)
  ;   Files1 = NewFiles2
  ),

  % Include directories from this recursion level.
  (   option(include_directories(true), Opts2)
  ->  append(Files1, NewDirs, Files2)
  ;   Files2 = Files1
  ),

  % Include the parent directory.
  (   IncludeSelf == true
  ->  Files3 = [Dir|Files2]
  ;   Files3 = Files2
  ).



%! directory_subdirectories(+Dir:atom, +Subdirs:list(atom)) is semidet.
%! directory_subdirectories(+Dir:atom, -Subdirs:list(atom)) is det.
%! directory_subdirectories(-Dir:atom, +Subdirs:list(atom)) is det.
% Relates a directory name to its subdirectory names.
%
% Occurrences of `..` in Dir are resolved.
%
% For absolute directory names the first subdirectory name is the empty atom.

directory_subdirectories(Dir, Subdirs):-
  nonvar(Dir), !,
  atomic_list_concat(Subdirs0, /, Dir),
  resolve_double_dots(Subdirs0, Subdirs).
directory_subdirectories(Dir, Subdirs0):-
  nonvar(Subdirs0), !,
  resolve_double_dots(Subdirs0, Subdirs),
  atomic_list_concat(Subdirs, /, Dir).
directory_subdirectories(_, _):-
  instantiation_error(_).

%! resolve_double_dots(
%!   +Subdirs:list(atom),
%!   -ResoledSubdirs:list(atom)
%! ) is det.

resolve_double_dots([], []).
resolve_double_dots([_,'..'|T1], T2):-
  resolve_double_dots(T1, T2).
resolve_double_dots([H|T1], [H|T2]):-
  resolve_double_dots(T1, T2).





% HELPERS %

%! nonfile_entry(+Entry:atom) is semidet.
%! nonfile_entry(-Entry:atom) is multi.

nonfile_entry(.).
nonfile_entry('..').
