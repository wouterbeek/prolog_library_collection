:- module(
  file_ext,
  [
    absolute_directory_name/3,  % +Spec, +Mode, -Dir
    cat/2,                      % +UriSpecs, +FileSpec
    create_date_directory/2,    % +Spec, -Dir
    create_date_time_file/2,    % +Spec, -File
    create_date_time_file/3,    % +Spec, +Exts, -File
    create_file/1,              % +File
    create_file_link/2,         % +From, +To
    create_time_file/2,         % +Spec, -File
    create_time_file/3,         % +Spec, +Exts, -File
    current_directory/1,        % ?Dir
    delete_directory_silent/1,  % +Dir
    delete_directory_and_contents_silent/1, % +Dir
    directory_recursive/2,      % +Dir, -Subdir
    file_age/2,                 % +File, -Age:float
    file_change_extension/3,    % +File1, +Ext, File2
    file_is_ready/1,            % +File
    file_is_ready/2,            % +File1, +File2
    file_name/2,                % ?File, ?Name
    file_paths/2,               % +File, -Paths
    file_ready/2,               % +File, -ReadyFile
    file_ready_time/2,          % +File, -ReadyTime
    file_size/2,                % +File, -Size
    file_touch_ready/1,         % +File
    is_file_link/1,             % +File
    is_fresh_age/2,             % +Age, +FreshnessLifetime
    is_fresh_file/2,            % +File, +FreshnessLifetime
    is_image_file/1,            % @Term
    is_older_file/2,            % +Path1, +Path2
    is_stale_age/2,             % +Age, +FreshnessLifetime
    is_stale_file/2,            % +File, +FreshnessLifetime
    is_younger_file/2,          % +Path1, +Path2
    latest_date_time_file/2,    % +Spec, -File
    latest_file/2,              % +Files, -LatestFile
    run_in_directory/2,         % :Goal_0, +Dir
    thread_file/1,              % -ThreadFile
    thread_file/2,              % +File, -ThreadFile
    wc/2,                       % +Source, -NumLines
    wildcard_file/2             % +Wildcard, -File
  ]
).
:- reexport(library(filesex)).

/** <module> File extensions

Extensions to the file operations in the standard SWI-Prolog libraries.

This module provides support predicates for creating, opening,
removing, and searching files.

These predicates are intended to be used in addition to [the
SWI-Prolog file
build-ins](http://www.swi-prolog.org/pldoc/man?section=files) and
[`library(filesex)`](http://www.swi-prolog.org/pldoc/man?section=filesex).

# Concepts

  - *File*
  - *Directory*
  - *File link*

# Terms

I am not aware of a standardized vocabulary about files.  Here is my
ad-hoc attempt:

  - *Absolute path*
    A path whose first character is the root character (`/').

  - *Base file name*
    An atom.

  - *Directory name*
    A file name that is a sequence of atoms separated by directory
    separators.  Every prefix of a directory name that ends at a
    directory separator denoted a directory.

  - *File extension*
    An atom.

  - *Local file name*
    A base file name that is optionally followed by the file extension
    separator (`.') and a file extension.

  - *Path*
    A file name that consists of directories separated by directory
    separators, and optionally a local file name.  If a local file
    name is present, the path denotes a file.  If no local file name is
    present, the path denotes a directory.

  - *Relative path*
    A path whose first character is not the root character (`/').

---

@author Wouter Beek
@version 2015/07-2017/04, 2017/08
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(error)).
:- use_module(library(filesex)).
:- use_module(library(http/http_ext)).
:- use_module(library(io)).
:- use_module(library(lists)).
:- use_module(library(md5)).
:- use_module(library(os_ext)).
:- use_module(library(thread_ext)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(string_ext)).
:- use_module(library(uri/uri_ext)).
:- use_module(library(uuid)).
:- use_module(library(yall)).

:- dynamic
    error:has_type/2,
    user:prolog_file_type/2.

:- meta_predicate
    run_in_directory(0, +).

:- multifile
    error:has_type/2,
    user:prolog_file_type/2.

user:prolog_file_type(bmp, image).
user:prolog_file_type(gif, image).
user:prolog_file_type(jpeg, image).
user:prolog_file_type(jpg, image).
user:prolog_file_type(png, image).

error:has_type(absolute_path, Term) :-
  error:has_type(atom, Term),
  os_root_prefix(Root),
  atom_concat(Root, _, Term).





%! absolute_directory_name(+Spec, +Mode, -Dir) is semidet.
%
% @tbd Requires that Dir exists in write modes.

absolute_directory_name(Spec, Mode, Dir) :-
  absolute_file_name(Spec, Dir, [access(Mode),file_type(directory)]).



%! create_date_directory(+Spec, -Dir) is det.
%
% Create and return the current date subdirectory of the given
% absolute directory name.
%
% Example: `/tmp' ⇒ `/tmp/2013/05/10'

create_date_directory(Spec, Dir) :-
  get_time(TS),
  format_time(atom(D), "%d", TS),
  format_time(atom(Mo), "%m", TS),
  format_time(atom(Y), "%Y", TS),
  absolute_file_name(Spec, PrefixDir, [access(write),file_type(directory)]),
  atomics_to_string([Y,Mo,D], "/", PostfixDir),
  directory_file_path(PrefixDir, PostfixDir, Dir),
  make_directory_path(Dir).



%! common_prefix_path(
%!   +Path1:atom,
%!   +Path2:atom,
%!   +CommonPrefixPath:atom
%! ) is semidet.
%! common_prefix_path(
%!   +Path1:atom,
%!   +Path2:atom,
%!   -CommonPrefixPath:atom
%! ) is det.
% Succeeds id Path1 and Path2 share the same CommonPrefixPath.

common_prefix_path(Path1, Path2, CommonPrefixPath) :-
  directory_subdirectories(Path1, PathComponents1),
  directory_subdirectories(Path2, PathComponents2),
  common_list_prefix(PathComponents1, PathComponents2, CommonComponentPrefix),
  directory_subdirectories(CommonPrefixPath, CommonComponentPrefix).



%! create_date_time_file(+Spec, -File) is det.
%! create_date_time_file(+Spec, +Exts, -File) is det.

create_date_time_file(Spec, File) :-
  create_date_directory(Spec, Dir),
  create_time_file(Dir, File).


create_date_time_file(Spec, Exts, File) :-
  create_date_time_file(Spec, Base),
  file_name_extensions(File, Base, Exts).



%! create_file(+File) is det.
%
% @throws type_error

create_file(File) :-
  exists_file(File), !.
create_file(File) :-
  is_absolute_file_name(File), !,
  touch(File).
create_file(File) :-
  type_error(absolute_file_name, File).



%! create_file_link(+From, +To) is det.
%
% Create a symbolic link pointing from file From in to file To.
%
% @throws existence_error if the linked to file (To) does not exist.

% File link already exists.
create_file_link(From, _) :-
  exists_file(From), !.
create_file_link(From, To) :-
  (exists_file(To) -> true ; existence_error(file, To)),
  create_file_directory(From),
  create_file_directory(To),
  link_file(To, From, symbolic).



%! create_time_file(+Spec, -File) is det.
%! create_time_file(+Spec, +Exts, -File) is det.

create_time_file(Spec, File) :-
  absolute_directory_name(Spec, write, Dir),
  get_time(TS),
  format_time(string(H), "%H", TS),
  format_time(string(Mi), "%M", TS),
  format_time(string(S), "%S", TS),
  atomics_to_string([H,Mi,S], "_", Local),
  directory_file_path(Dir, Local, File).


create_time_file(Spec, Exts, File) :-
  create_time_file(Spec, Base),
  file_name_extensions(File, Base, Exts).



%! current_directory(+Dir) is semidet.
%! current_directory(-Dir) is det.

current_directory(Dir) :-
  absolute_file_name(., Dir, [file_type(directory)]).



%! delete_directory_silent(+Dir) is det.

delete_directory_silent(Dir) :-
  exists_directory(Dir), !,
  delete_directory(Dir).
delete_directory_silent(_).



%! delete_directory_and_contents_silent(+Dir) is det.

delete_directory_and_contents_silent(Dir) :-
  exists_directory(Dir), !,
  delete_directory_and_contents(Dir).
delete_directory_and_contents_silent(_).



%! directory_recursive(+Dir, -Subdir) is det.

directory_recursive(Dir, Subdir) :-
  directory_path(Dir, Subdir0),
  exists_directory(Subdir0),
  (   directory_recursive(Subdir0, Subdir)
  ;   Subdir = Subdir0
  ).



%! file_age(+File, -Age:float) is det.

file_age(File, Age) :-
  time_file(File, LastModified),
  get_time(Now),
  Age is Now - LastModified.



%! file_change_extension(+From, +Ext, -To) is det.
%
% To is like From but has file extension Ext.  Also succeeds if From
% has no file extension.

file_change_extension(From, Ext, To) :-
  file_name_extension(Base, _, From),
  file_name_extension(Base, Ext, To).



%! file_components(+Path:atom, +Directory:atom, +Base:atom,
%!                 ?Extensions:list(atom)) .
%
% Relates a file path to its components:
%
%   - directory
%   - base name
%   - file extensions
%
% For directories, the base name and file extension are
% uninstantiated.

file_components(Path, Directory, Base, Exts) :-
  var(Path), !,
  must_be(atom, Directory),
  file_local_components(Local, Base, Exts),
  directory_file_path(Directory, Local, Path).
file_components(Path, Directory, Base, Exts) :-
  directory_file_path(Directory, Local, Path),
  file_local_components(Base, Ext, Local)

file_local_components(Local, Base, Exts) :-
  atomic_list_concat([Base|Exts], ., Local).



%! file_is_ready(+File) is semidet.

file_is_ready(File) :-
  file_ready(File, Ready),
  exists_file(Ready).



%! file_is_ready(+File1, +File2) is semidet.

file_is_ready(File1, File2) :-
  file_ready_time(File2, Ready2),
  file_ready_time(File1, Ready1),
  Ready2 >= Ready1.



%! file_name(+File, -Name) is det.
%! file_name(-File, +Name) is det.

file_name(File, Name) :-
  file_name_extensions(File, Name, _).



%! file_paths(+File, -Paths:list(atom)) is det.

file_paths(File, Paths) :-
  atomic_list_concat(Paths, /, File).



%! file_ready(+File, -ReadyFile) is det.

file_ready(File, ReadyFile) :-
  atomic_list_concat([File,ready], ., ReadyFile).



%! file_ready_time(+File, -ReadyTime) is det.

file_ready_time(File, ReadyTime) :-
  file_ready(File, ReadyFile),
  exists_file(ReadyFile),
  time_file(ReadyFile, ReadyTime).



%! file_size(+File, -Size) is det.
%
% @see Sane name for size_file/2.

file_size(File, Size) :-
  size_file(File, Size).



%! file_touch_ready(+File) is det.

file_touch_ready(File) :-
  file_ready(File, ReadyFile),
  touch(ReadyFile).



%! hidden_file_name(+Path:atom, +Hidden:atom) is semidet.
%! hidden_file_name(+Path:atom, -Hidden:atom) is det.
%
% Returns the hidden file name for the given atomic name.

hidden_file_name(Path, Hidden) :-
  file_components(Path, Dir, Base, Ext),
  atomic_concat(., Base, HiddenBase),
  file_components(Hidden, Dir, HiddenBase, Ext).



%! is_file_link(+File) is semidet.

is_file_link(File) :-
  read_link(File, _, _).



%! is_fresh_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_fresh_age(_, inf) :- !.
is_fresh_age(Age, FreshnessLifetime) :-
  Age =< FreshnessLifetime.



%! is_fresh_file(+File, +FreshnessLifetime:between(0.0,inf)) is semidet.

is_fresh_file(File, FreshnessLifetime) :-
  file_age(File, Age),
  is_fresh_age(Age, FreshnessLifetime).



%! is_image_file(+File) is semidet.
%
% Determines whether a file stores an image or not based on the file
% extension.

is_image_file(File) :-
  file_extensions(File, Exts),
  once((
    member(Ext, Exts),
    user:prolog_file_type(Ext, image)
  )).
  


%! is_older_file(+Path1, +Path2) is semidet.

is_older_file(Path1, Path2) :-
  is_younger_file(Path2, Path1).



%! is_stale_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_stale_age(_, inf) :- !, fail.
is_stale_age(Age, FreshnessLifetime) :-
  Age > FreshnessLifetime.



%! is_stale_file(+File, +FreshnessLifetime:between(0.0,inf)) is semidet.

is_stale_file(File, FreshnessLifetime) :-
  file_age(File, Age),
  is_stale_age(Age, FreshnessLifetime).



%! is_younger_file(+Path1, +Path2) is semidet.
% Succeeds if the file denoted by Path1 is younger than
% the file denoted by Path2.

is_younger_file(Path1, Path2) :-
  time_file(Path1, T1),
  time_file(Path2, T2),
  T1 > T2.



%! latest_date_time_file(+Spec, -File) is det.

latest_date_time_file(Spec, Latest) :-
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



%! latest_file(+Files:list(atom), -Latest) is det.
%
% Returns the most recently created or altered file from within a list
% of files.

latest_file([H|T], Latest) :-
  time_file(H, Time),
  latest_file(T, Time-H, Latest).

latest_file([], _-Latest, Latest).
latest_file([H|T], Time1-File1, Latest) :-
  time_file(H, NewTime),
  (   NewTime > Time1
  ->  Time2 = NewTime,
      File2 = H
  ;   Time2 = Time1,
      File2 = File1
  ),
  latest_file(T, Time2-File2, Latest).



%! run_in_directory(:Goal_0, +Dir) is det.

run_in_directory(Goal_0, Dir) :-
  working_directory(Dir0, Dir),
  Goal_0,
  working_directory(Dir, Dir0).



%! thread_file(-ThreadFile) is det.
%! thread_file(+File, -ThreadFile) is det.
%
% Returns a thread-specific file name based on the given file name.

thread_file(File) :-
  uuid(Base),
  thread_file(Base, File).


thread_file(Base, File) :-
  thread_name(ThreadName),
  % @note The thread name may not be a legal file name.
  md5_hash(ThreadName, Hash, []),
  file_name_extension(Base, Hash, File).



%! wildcard_file(+Wildcard, -File) is nondet.

wildcard_file(Wildcard, File) :-
  expand_file_name(Wildcard, Files),
  member(File, Files).





% MESSAGES %

:- multifile
    prolog:message//1.

prolog:message(delete_directory(Dir)) -->
  ["Deleting directory ~a."-[Dir]].
prolog:message(delete_directory_and_contents(Dir)) -->
  ["Deleting contents of directory ~a."-[Dir]].
prolog:message(delete_file(File)) -->
  ["Deleting file ~a."-[File]].
