:- module(dir_ext,
  [
    copy_directory/3, % +FromDir:atom
                      % +ToDir:atom
                      % +Options:list(nvpair)
    delete_directory/2, % +Dir:atom
                        % +Options:list(nvpair)
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

:- predicate_options(copy_directory/3, 3, [
     pass_to(delete_directory/2, 2)
   ]).
:- predicate_options(delete_directory/2, 2, [
     include_self(+boolean)
   ]).





%! copy_directory(+FromDir:atom, +ToDir:atom, +Options:list(nvpair)) is det.

copy_directory(FromDir, ToDir, Options):-
  delete_directory(ToDir, Options),
  copy_directory(FromDir, ToDir).



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
  memberchk(Last, [.,'..']), !.
% Files.
delete_directory(File, _):-
  delete_file(File).



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
