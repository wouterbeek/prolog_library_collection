:- module(
  file_ext,
  [
    absolute_directory_name/3,  % +Spec, +Mode, -Dir
    append_directories/2,       % +Dirs, -Dir
    append_directories/3,       % +Dir1, +Dir2, -Dir
    count_numlines/2,           % +Source, -NumLines
    create_directory/1,         % +Dir
    create_file/1,              % +File
    create_file_directory/1,    % +File
    create_file_link/2,         % +From, +To
    current_directory/1,        % ?Dir
    delete_directory_msg/1,     % +Dir
    delete_directory_and_contents_msg/1, % +Dir
    delete_file_msg/1,          % +File
    directory_file/2,           % +Dir, -File
    directory_is_empty/1,       % +Dir
    directory_path/2,           % +Dir, -Path
    directory_path_recursive/2, % +Dir, -Path
    directory_recursive/2,      % +Dir, -Subdir
    directory_subdirectories/2, % ?Dir, ?Subdirs
    file_age/2,                 % +File, -Age:float
    file_change_extension/3,    % +File1, +Ext, File2
    file_extensions/2,          % +File, -Exts
    file_name/2,                % ?File, ?Name
    file_name_extensions/3,     % ?File, ?Name, ?Exts
    file_paths/2,               % +File, -Paths
    file_size/2,                % +File, -Size
    is_dummy_file/1,            % +File
    is_fresh_age/2,             % +Age, +FreshnessLifetime
    is_fresh_file/2,            % +File, +FreshnessLifetime
    is_stale_age/2,             % +Age, +FreshnessLifetime
    is_stale_file/2,            % +File, +FreshnessLifetime
    latest_file/2,              % +Files, -LatestFile
    root_prefix/1,              % ?Prefix
    run_in_directory/2,         % :Goal_0, +Dir
    thread_file/1,              % -ThreadFile
    thread_file/2,              % +File, -ThreadFile
    touch/1,                    % +File
    wd/1,                       % ?Dir
    wildcard_file/2             % +Wildcard, -File
  ]
).
:- reexport(library(filesex)).

/** <module> File extensions

Extensions to the file operations in the standard SWI-Prolog libraries.

@author Wouter Beek
@version 2015/07-2015/11, 2016/01-2016/03, 2016/05-2016/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(filesex)).
:- use_module(library(http/http_ext)).
:- use_module(library(lists)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io)).
:- use_module(library(os/os_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(uuid)).

:- meta_predicate
    run_in_directory(0, +).





%! absolute_directory_name(+Spec, +Mode, -Dir) is semidet.

absolute_directory_name(Spec, Mode, Dir) :-
  absolute_file_name(Spec, Dir, [access(Mode),file_type(directory)]).



%! append_directories(+Dirs, -Dir) is det.

append_directories([], '/') :- !.
append_directories([Dir], Dir) :- !.
append_directories([H|T], Dir) :-
  append_directories0(H, T, Dir).

append_directories0(Dir, [], Dir) :- !.
append_directories0(Dir1, [H|T], Dir) :-
  append_directories(Dir1, H, Dir2),
  append_directories0(Dir2, T, Dir).


%! append_directories(+Dir1, +Dir2, -Dir) is det.
%
% Returns the directory name obtained by concatenating the given
% directory names.
%
% The empty atom in the first position indicates the root directory.
%
% Does *not* ensure that any of the directories exist.

append_directories(Dir1, Dir2, Dir3) :-
  directory_subdirectories(Dir1, Subdirs1),
  directory_subdirectories(Dir2, Subdirs2),
  append(Subdirs1, Subdirs2, Subdirs3),
  directory_subdirectories(Dir3, Subdirs3).



%! count_numlines(+Source, -NumLines) is det.

count_numlines(Source, NumLines) :-
  call_on_stream(
    Source,
    count_numlines_stream0(NumLines),
    [metadata(Path)]
  ),
  writeln(Path).


count_numlines_stream0(NumLines, In, L, L) :-
  count_numlines_stream0(In, 0, NumLines).


count_numlines_stream0(In, M1, NumLines) :-
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  NumLines = M1
  ;   M2 is M1 + 1,
      count_numlines_stream0(In, M2, NumLines)
  ).



%! create_directory(+Dir) is det.

create_directory(Dir) :-
  exists_directory(Dir), !.
create_directory(Dir) :-
  make_directory(Dir).



%! create_file(+File) is det.
% @throws type_error

create_file(File) :-
  exists_file(File), !.
create_file(File) :-
  is_absolute_file_name(File), !,
  touch(File).
create_file(File) :-
  type_error(absolute_file_name, File).



%! create_file_directory(+Path) is det.
% Ensures that the directory structure for the given file exists.

create_file_directory(Path) :-
  (exists_directory(Path) -> Dir = Path ; directory_file_path(Dir, _, Path)),
  make_directory_path(Dir).



%! create_file_link(+From, +To) is det.
%
% Create a symbolic link pointing from file From in to file To.

create_file_link(From, To) :-
  exists_file(To), !,
  create_file_directory(From),
  create_file_directory(To),
  link_file(To, From, symbolic).
create_file_link(_, _).



%! current_directory(+Dir) is semidet.
%! current_directory(-Dir) is det.

current_directory(Dir) :-
  absolute_file_name(., Dir, [file_type(directory)]).



%! delete_directory_msg(+Dir) is det.

delete_directory_msg(Dir) :-
  exists_directory(Dir), !,
  print_message(informational, delete_directory(Dir)),
  delete_directory(Dir).
delete_directory_msg(_).



%! delete_directory_and_contents_msg(+Dir) is det.

delete_directory_and_contents_msg(Dir) :-
  exists_directory(Dir), !,
  print_message(informational, delete_directory_and_contents(Dir)),
  delete_directory_and_contents(Dir).
delete_directory_and_contents_msg(_).



%! delete_file_msg(+File) is det.
%
% Succeed silently if File does not exist and print a message when
% it does exist and is deleted.

delete_file_msg(File) :-
  exists_file(File), !,
  print_message(informational, delete_file(File)),
  delete_file(File).
delete_file_msg(_).



% directory_file(+Dir, -File) is nondet.
%
% Non-deterministic variant of directory_files/2 that skips dummy
% files.

directory_file(Dir, File) :-
  directory_files(Dir, Files),
  member(File, Files),
  \+ is_dummy_file(File).



%! directory_is_empty(+Dir) is semidet.

directory_is_empty(Dir) :-
  \+ directory_file(Dir, _).



%! directory_path(+Dir, -Path) is nondet.

directory_path(Dir, Path) :-
  directory_file(Dir, File),
  directory_file_path(Dir, File, Path).



%! directory_path_recursive(+Dir, -Path) is nondet.

directory_path_recursive(Dir, Path) :-
  directory_path(Dir, Path0),
  (   exists_directory(Path0)
  ->  directory_path_recursive(Path0, Path)
  ;   Path = Path0
  ).



%! directory_recursive(+Dir, -Subdir) is det.

directory_recursive(Dir, Subdir) :-
  directory_path(Dir, Subdir0),
  exists_directory(Subdir0),
  (   directory_recursive(Subdir0, Subdir)
  ;   Subdir = Subdir0
  ).



%! directory_subdirectories(+Dir, -Subdirs) is det.
%! directory_subdirectories(-Dir, +Subdirs) is det.
%
% Occurrences of `.` and `..` in Dir are resolved.
%
% The empty atom in the first position indicates the root directory.
%
% For absolute directory names the first subdirectory name is the
% empty atom.

directory_subdirectories(Dir, Subdirs2) :-
  ground(Dir), !,
  atomic_list_concat(Subdirs1, /, Dir),
  resolve_subdirectories(Subdirs1, Subdirs2).
directory_subdirectories(Dir, Subdirs1) :-
  resolve_subdirectories(Subdirs1, Subdirs2),
  atomic_list_concat(Subdirs2, /, Dir).



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



%! file_extensions(+File, -Exts) is det.

file_extensions(File, Exts) :-
  file_name_extensions(File, _, Exts).



%! file_name(+File, -Name) is det.
%! file_name(-File, +Name) is det.

file_name(File, Name) :-
  file_name_extensions(File, Name, _).



%! file_name_extensions(+File, -Name, -Exts) is det.
%! file_name_extensions(-File, +Name, +Exts) is det.

file_name_extensions(File, Name, Exts) :-
  atomic_list_concat([Name|Exts], ., File).



%! file_paths(+File, -Paths:list(atom)) is det.

file_paths(File, Paths) :-
  atomic_list_concat(Paths, /, File).



%! file_size(+File, -Size) is det.
% @see Sane name for size_file/2.

file_size(File, Size) :-
  size_file(File, Size).



%! is_dummy_file(+File) is semidet.

is_dummy_file(.).
is_dummy_file(..).



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



%! latest_file(+Files:list(atom), -Latest) is det.
% Returns the most recently created or altered file from within a list of
% files.

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



%! root_prefix(+Prefix) is semidet.
%! root_prefix(-Prefix) is multi.

:- if(os(unix)).
root_prefix(/).
:- endif.
:- if(os(windows)).
root_prefix('C:\\').
:- endif.



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
  thread_name(Thread),
  file_name_extension(Base, Thread, File).



%! touch(+File) is det.

touch(File) :-
  setup_call_cleanup(open(File, write, Write), true, close(Write)).



%! wd(+Dir) is semidet.
%! wd(-Dir) is det.

wd(Dir) :-
  working_directory(Dir, Dir).



%! wildcard_file(+Wildcard, -File) is nondet.

wildcard_file(Wildcard, File) :-
  expand_file_name(Wildcard, Files),
  member(File, Files).





% HELPERS %

%! resolve_subdirectories(+Subdirs, -ResoledSubdirs) is det.

resolve_subdirectories([], []) :- !.
resolve_subdirectories([''], []) :- !.
resolve_subdirectories([.|T1], T2) :- !,
  resolve_subdirectories(T1, T2).
resolve_subdirectories([_,..|T1], T2) :- !,
  resolve_subdirectories(T1, T2).
resolve_subdirectories([H|T1], [H|T2]) :-
  resolve_subdirectories(T1, T2).





% MESSAGES %

:- multifile
    prolog:message//1.

prolog:message(delete_directory(Dir)) -->
  ["Deleting directory ~a."-[Dir]].
prolog:message(delete_directory_and_contents(Dir)) -->
  ["Deleting contents of directory ~a."-[Dir]].
prolog:message(delete_file(File)) -->
  ["Deleting file ~a."-[File]].
