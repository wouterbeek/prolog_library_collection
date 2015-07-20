:- module(dir_ext,
  [
    append_directories/3, % +Dir1:atom
                          % +Dir2:atom
                          % -Dir3:atom
    copy_directory/3, % +FromDir:atom
                      % +ToDir:atom
                      % +Options:list(nvpair)
    create_directory/1, % +Directory:atom
    create_directory/2, % +CurrentSpec:compound
                        % +Subdirs:list(atom)
    create_directory/3, % +CurrentSpec:compound
                        % +Subdirs:list(atom)
                        % -AbsoluteDir:atom
    delete_directory/2, % +Dir:atom
                        % +Options:list(nvpair)
    directory_files/3, % +Dir:atom
                       % -AbsFiles:list(atom)
                       % +Options:list(nvpair)
    directory_subdirectories/2, % ?Dir:atom
                                % ?Subdirs:list(atom)
    link_directory_contents/2, % +FromDir:atom
                               % +ToDir:atom
    run_in_working_directory/2 % :Call
                               % +WorkingDir:atom
  ]
).

/** <module> Directory extensions

Extensions for handling directories.

@author Wouter Beek
@version 2013/06-2013/07, 2013/09, 2013/11-2014/02, 2014/04-2014/05, 2014/08,
         2014/10, 2015/02
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lambda)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).

:- use_module(plc(io/file_ext)).
:- use_module(plc(prolog/pl_control)).

:- meta_predicate(directory_files(+,-,:)).
:- meta_predicate(run_in_working_directory(0,+)).

is_meta(order).

:- predicate_options(copy_directory/3, 3, [
     pass_to(delete_directory/2, 2)
   ]).
:- predicate_options(delete_directory/2, 2, [
     include_self(+boolean)
   ]).
:- predicate_options(directory_files/3, 3, [
     file_types(+list(atom)),
     include_directories(+boolean),
     include_self(+boolean),
     order(+oneof([lexicographic])),
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



%! copy_directory(+FromDir:atom, +ToDir:atom, +Options:list(nvpair)) is det.

copy_directory(FromDir, ToDir, Options):-
  delete_directory(ToDir, Options),
  copy_directory(FromDir, ToDir).



%! create_directory(+Abs:atom) is det.
% Creates a directory with the given absolute file path.
%
% This predicate does not support argument `Spec` argument
% since absolute_file_name/3 does not work
% with path names that denote non-existing directories.
%
% @see http://www.swi-prolog.org/pldoc/doc_for?object=absolute_file_name/3

% Current directory: nothing to create.
create_directory('.'):- !.
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

%! create_directory(+CurrentSpec:compound, +Subdirs:list(atom)) is det.

create_directory(CurrentSpec, Subdirs):-
  create_directory(CurrentSpec, Subdirs, _).

%! create_directory(
%!   +CurrentSpec:compound,
%!   +Subdirs:list(atom),
%!   -AbsoluteDir:atom
%! ) is det.
% Creates a nested directory based on:
%   - a current directory
%   - a list of consecutive subdirectories

create_directory(CurrentSpec, Subdirs, Abs):-
  absolute_file_name(
    CurrentSpec,
    CurrentDir,
    [access(write),file_type(directory)]
  ),
  create_directory0(CurrentDir, Subdirs, Abs).

create_directory0(Abs, [], Abs):- !.
create_directory0(Current, [Sub|Subs], Abs):-
  directory_file_path(Current, Sub, Dir),
  xor(
    exists_directory(Dir),
    make_directory(Dir)
  ),
  create_directory0(Dir, Subs, Abs).



%! delete_directory(+Dir:atom, +Options:list(nvpair)) is det.
% Deletes all file in the given directory that are of the given file type.
%
% The following options are supported:
%   - include_self(+IncludeOrNot:boolean)

% Directories.
delete_directory(Dir, Options):-
  exists_directory(Dir), !,
  directory_files(
    Dir,
    Children,
    [include_directories(true),include_self(false),recursive(false)]
  ),
  maplist(\Child^delete_directory(Child, Options), Children),
  (   option(include_self(true), Options)
  ->  delete_directory(Dir),
      debug(dir_ext, 'Dir ~w was deleted.', [Dir])
  ;   true
  ).
% Non-file entries.
delete_directory(Dir, _):-
  directory_subdirectories(Dir, SubDirs),
  last(SubDirs, Last),
  memberchk(Last, ['.','..']), !.
% Files.
delete_directory(File, _):-
  delete_file(File).



%! directory_files(
%!   +Dir:atom,
%!   -AbsoluteFiles:list(atom),
%!   +Options:list(nvpair)
%! ) is det.
% Variant of directory_files/2 that returns absolute file names
% instead of relative ones and excludes non-file entries.
%
% The following options are supported:
%   * `file_types(+FileTypes:list(atom))`
%     A list of atomic file types that are used as a filter.
%     Default: no file type filter.
%   * `include_directories(+IncludeDirectories:boolean)`
%     Whether (sub)directories are included or not.
%     Default: `false`.
%   * `include_self(+IncludeSelf:boolean)`
%     Whether or not the enclosing directory is included.
%     Default: `false`.
%   * `order(+Order:oneof([lexicographic,none]))`
%     The order in which the files are returned.
%     Lexicographic order uses ordsets.
%     Default: `none`.
%   * `recursive(+Recursive:boolean)`
%     Whether subdirectories are searched recursively.
%     Default: `true`.

directory_files(Dir, Files4, Options1):-
  meta_options(is_meta, Options1, Options2),

  % Note that the list of files is *not* ordered!
  directory_files(Dir, New1),

  % Remove `.` and `..`.
  exclude(nonfile_entry, New1, New2),

  % Make the file names absolute.
  maplist(directory_file_path(Dir), New2, New3),

  partition(exists_directory, New3, NewDirectories, NewFiles1),

  % Filter based on a list of file types, if given.
  (   option(file_types(FileTypes), Options2)
  ->  include(
        \File^(
          file_component(File, file_type, FileType),
          memberchk(FileType, FileTypes)
        ),
        NewFiles1,
        NewFiles2
      )
  ;   NewFiles2 = NewFiles1
  ),

  % Make sure the `include_self` option is excluded from
  % the processing of subdirectories.
  select_option(include_self(IncludeSelf), Options2, Options3, false),

  % Include directories and files from deeper recursion levels.
  (   option(recursive(true), Options3)
  ->  maplist(
        \NewDir^NewFiles^directory_files(NewDir, NewFiles, Options3),
        NewDirectories,
        NewFiless
      ),
      append([NewFiles2|NewFiless], Files1)
  ;   Files1 = NewFiles2
  ),

  % Include directories from this recursion level.
  (   option(include_directories(true), Options3)
  ->  append(Files1, NewDirectories, Files2)
  ;   Files2 = Files1
  ),

  % Include the parent directory.
  (   IncludeSelf == true
  ->  Files3 = [Dir|Files2]
  ;   Files3 = Files2
  ),

  % Apply requested ordering.
  option(order(Order), Options3, =),
  call(Order, Files3, Files4).



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

%! resolve_double_dots(+Subdirs:list(atom), -ResoledSubdirs:list(atom)) is det.

resolve_double_dots([], []).
resolve_double_dots([_,'..'|T1], T2):-
  resolve_double_dots(T1, T2).
resolve_double_dots([H|T1], [H|T2]):-
  resolve_double_dots(T1, T2).



%! link_directory_contents(+FromDir:atom, +ToDir:atom) is det.
% Creates symbolic links in `ToDir` for all the files in `FromDir`.

link_directory_contents(FromDir, ToDir):-
  directory_files(
    FromDir,
    Files,
    [include_directories(false),include_self(false),recursive(true)]
  ),
  maplist(\File^create_file_link(File, ToDir), Files).



%! run_in_working_directory(:Goal, +WorkingDirectory:atom) is det.

run_in_working_directory(Goal, Dir1):-
  working_directory(Dir1, Dir2),
  call(Goal),
  working_directory(Dir2, Dir1).





% HELPERS

%! nonfile_entry(+Entry:atom) is semidet.

nonfile_entry('.').
nonfile_entry('..').

