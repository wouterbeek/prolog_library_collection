:- module(
  uri_file_name,
  [
    uri_file_name_nested/2, % +Uri, -File
    uri_file_name_nested/3  % +Uri, -File, Opts
  ]
).

/** <module> URI file name

Support for creating local file names that are somewhat similar to URIs.

This comes in handy when downloading files locally,
since the use of these file names allows a human reader
to relate them to the URI they were downloaded from.

@author Wouter Beek
@version 2015/08, 2016/01
*/

:- use_module(library(atom_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(option)).
:- use_module(library(uri)).





%! uri_file_name_nested(+Uri, -File) is det.
%! uri_file_name_nested(+Uri, -File, +Opts) is det.
%
% Succeeds if File is the name of a nested path denoting a regular
% file, where the name is based on the original URI.
%
% The purpose of this predicate is to create file names that resemble
% the URI as close as possible for a human reader to discern.
%
% The following options are supported:
%
%   * directory(+atom) The directory relative to which the URI-based
%   files are stored.  This is either (1) an absolute file name of a
%   directory file, or (2) a relative file name of a directory file,
%   or (3) a specification of the form `outer(inner)`, where `outer`
%   can be resolved using `user:file_search_path/2` declarations.
%
% @tbd Since URI paths are more expressive than Linux paths it does
% not seem possible to download URI `.../a/` and `.../a` as both would
% be mapped onto file `.../a`.  We therefore store directory-like URI
% paths in a file called `directory_dummy`.

uri_file_name_nested(Uri, File) :-
  uri_file_name_nested(Uri, File, []).


uri_file_name_nested(Uri, File, Opts) :-
  current_directory(CurrentDir),
  option(dir(ParentDir0), Opts, CurrentDir),
  uri_components(Uri, uri_components(Scheme,Authority,Path,_,_)),

  % Make sure the path ends in a non-directory file.
  (   sub_atom(Path, _, 1, 0, /)
  ->  PathDir = Path,
      Base = directory_dummy
  ;   file_directory_name(Path, PathDir),
      file_base_name(Path, Base)
  ),

  % Use (1) the URI scheme, (2) the URI authority,
  % and (3) the directory-part of the URI path to construct
  % the directory of the URI-based file.
  directory_subdirectories(PathDir, [''|PathDirs]),
  directory_subdirectories(UriDir, [Scheme,Authority|PathDirs]),

  % Resolve the parent directory input to an absolute file name.
  absolute_file_name(
    ParentDir0,
    ParentDir,
    [access(write),file_type(directory)]
  ),
  atom_ending_in(ParentDir, /, ParentDirSlash),

  % The URL path is now created relative to the parent directory.
  relative_file_name(Dir, ParentDirSlash, UriDir),

  % Make sure the directory exists.
  make_directory_path(Dir),

  % Return the file.
  directory_file_path(Dir, Base, File).
