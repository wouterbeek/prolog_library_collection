:- module(
  uri_file_name,
  [
    nested_uri_file_name/2, % +Uri, -File
    nested_uri_file_name/3 % +ParentDirectory:atom
                           % +Uri:atom
                           % -File:atom
  ]
).

/** <module> URI file name

Support for creating local file names that are somewhat similar to URIs.

This comes in handy when downloading files locally,
since the use of these file names allows a human reader
to relate them to the URI they were downloaded from.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(atom_ext)).
:- use_module(library(os/dir_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(uri)).





%! nested_uri_file_name(+Uri:atom, -File:atom) is det.
% Wrapper around nested_uri_file_name/3
% that uses the current directory as the parent directory.

nested_uri_file_name(Uri, File):-
  current_directory(Dir),
  nested_uri_file_name(Dir, Uri, File).


%! nested_uri_file_name(+ParentDirectory:atom, +Uri:atom, -File:atom) is det.
% Succeeds if File is the name of a nested path denoting a regular file,
% where the name is based on the original URI.
%
% The purpose of this predicate is to create file names that
% resemble the URI as close as possible for a human reader to discern.
%
% @arg ParentDirectory is the directory relative to which
%      the URI-based files are stored.
%      This is either (1) an absolute file name of a directory file,
%      or (2) a relative file name of a directory file,
%      or (3) a specification of the form `outer(inner)`,
%      where `outer` can be resolved using `user:file_search_path/2`
%      declarations.
% @arg Uri is a standards-compliant URL on which the file name is based.
% @arg File is a non-directory absolute file name,
%      whose directory either exists or is created by this predicate.
%
% @tbd Since URI paths are more expressive than Linux paths
%      it does not seem possible to download URI `.../a/` and `.../a`
%      as both would be mapped onto file `.../a`.
%      We therefore store directory-like URI paths in a file called
%      `dir_dummy`.

nested_uri_file_name(ParentDir0, Uri, File):-
  uri_components(Uri, uri_components(Scheme,Authority,Path,_,_)),

  % Make sure the path ends in a non-directory file.
  (   sub_atom(Path, _, 1, 0, '/')
  ->  PathDir = Path,
      Base = dir_dummy
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
