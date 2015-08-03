:- module(
  dir_ext,
  [
    directory_files/3 % +Directory:atom
                      % -AbsoluteFiles:list(atom)
                      % +Options:list(compound)
  ]
).

/** <module> Directory extensions

Extensions for handling directory files in SWI-Prolog.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).
:- use_module(library(filesex)).
:- use_module(library(lambda)).
:- use_module(library(lists)).
:- use_module(library(option)).





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
%   * order(+Order:oneof([lexicographic,none]))
%     The order in which the files are returned.
%     Lexicographic order uses ordsets.
%     Default is `none`.
%   * recursive(+Recursive:boolean)
%     Whether subdirectories are searched recursively.
%     Default is `true`.

directory_files(Dir, Files4, Opts1):-
  meta_options(is_meta, Opts1, Opts2),

  % Note that the list of files is *not* ordered!
  directory_files(Dir, New1),

  % Remove `.` and `..`.
  exclude(nonfile_entry, New1, New2),

  % Make the file names absolute.
  maplist(directory_file_path(Dir), New2, New3),

  partition(exists_directory, New3, NewDirs, NewFiles1),

  % Filter based on a list of file types, if given.
  (   option(file_types(FileTypes), Opts2)
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
  select_option(include_self(IncludeSelf), Opts2, Opts3, false),

  % Include directories and files from deeper recursion levels.
  (   option(recursive(true), Opts3)
  ->  maplist(
        \NewDir^NewFiles^directory_files(NewDir, NewFiles, Opts3),
        NewDirs,
        NewFiless
      ),
      append([NewFiles2|NewFiless], Files1)
  ;   Files1 = NewFiles2
  ),

  % Include directories from this recursion level.
  (   option(include_directories(true), Opts3)
  ->  append(Files1, NewDirs, Files2)
  ;   Files2 = Files1
  ),

  % Include the parent directory.
  (   IncludeSelf == true
  ->  Files3 = [Dir|Files2]
  ;   Files3 = Files2
  ),

  % Apply requested ordering.
  option(order(Order), Opts3, =),
  call(Order, Files3, Files4).





% HELPERS %

%! nonfile_entry(+Entry:atom) is semidet.
%! nonfile_entry(-Entry:atom) is multi.

nonfile_entry('.').
nonfile_entry('..').
