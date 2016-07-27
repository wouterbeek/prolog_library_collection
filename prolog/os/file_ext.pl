:- module(
  file_ext,
  [
    count_numlines/2,        % +Source, -NumLines
    create_file/1,           % +File
    create_file_directory/1, % +File
    create_file_link/2,      % +File, +Dir
    file_age/2,              % +File, -Age:float
    file_change_extension/3, % +File1, +Ext, File2
    file_paths/2,            % +File, -Paths
    file_size/2,             % +File, -Size
    is_fresh_age/2,          % +Age:between(0.0,inf), +FreshnessLifetime:between(0.0,inf)
    is_fresh_file/2,         % +File, +FreshnessLifetime:between(0.0,inf)
    is_stale_age/2,          % +Age:between(0.0,inf), +FreshnessLifetime:between(0.0,inf)
    is_stale_file/2,         % +File, +FreshnessLifetime:between(0.0,inf)
    latest_file/2,           % +Files, -LatestFile
    thread_file/1,           % -ThreadFile
    thread_file/2,           % +File, -ThreadFile
    touch/1,                 % +File
    root_prefix/1            % ?Prefix
  ]
).
:- reexport(library(filesex)).

/** <module> File extensions

Extensions to the file operations in the standard SWI-Prolog libraries.

@author Wouter Beek
@version 2015/07-2015/11, 2016/01-2016/03, 2016/05-2016/07
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(filesex)).
:- use_module(library(http/http_ext)).
:- use_module(library(lists)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/directory_ext)).
:- use_module(library(os/os_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(uuid)).





%! count_numlines(+Source, -NumLines) is det.

count_numlines(Source, N) :-
  catch(
    call_on_stream(Source, count_numlines_stream0(N)),
    error(no_content(_),_),
    N = 0
  ).


count_numlines_stream0(N, In, Meta, Meta) :-
  count_numlines_stream0(N, 0, In).


count_numlines_stream0(N, M1, In) :-
  read_line_to_codes(In, Cs),
  (   Cs == end_of_file
  ->  N = M1
  ;   M2 is M1 + 1,
      count_numlines_stream0(In, M2, N)
  ).



%! create_file(+File) is det.
% @throws type_error

create_file(File) :-
  exists_file(File), !.
create_file(File) :-
  is_absolute_file_name(File), !,
  touch(File).
create_file(File) :-
  type_error(absolute_file_name, File).



%! create_file_directory(+Path:atom) is det.
% Ensures that the directory structure for the given file exists.

create_file_directory(Path) :-
  (exists_directory(Path) -> Dir = Path ; directory_file_path(Dir, _, Path)),
  make_directory_path(Dir).



%! create_file_link(+File, +Dir) is det.
%
% Create a symbolic link pointing to File in Dir.
%
% The symbolic link has the same name as the file linked to.

create_file_link(Path, Dir) :-
  directory_file_path(_, File, Path),
  directory_file_path(Dir, File, Link),
  (exists_file(Link) -> true ; link_file(Path, Link, symbolic)).



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



%! file_paths(+File, -Paths:list(atom)) is det.

file_paths(File, Paths) :-
  atomic_list_concat(Paths, /, File).



%! file_size(+File, -Size) is det.
% @see Sane name for size_file/2.

file_size(File, Size) :-
  size_file(File, Size).



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



%! latest_file(+Files:list(atom), -Latest:atom) is det.
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



%! root_prefix(+Prefix:atom) is semidet.
%! root_prefix(-Prefix:atom) is multi.

:- if(os(unix)).
root_prefix(/).
:- endif.
:- if(os(windows)).
root_prefix('C:\\').
:- endif.



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
