:- module(
  file_ext,
  [
    create_file/1,           % +File
    create_file_directory/1, % +File
    file_age/2,              % +File, -Age:float
    file_extensions/2,       % +File, -Extensions
    file_name/2,             % +Metadata, -File
    file_paths/2,            % +File, -Paths
    is_fresh_age/2,          % +Age:between(0.0,inf), +FreshnessLifetime:between(0.0,inf)
    is_fresh_file/2,         % +File, +FreshnessLifetime:between(0.0,inf)
    is_stale_age/2,          % +Age:between(0.0,inf), +FreshnessLifetime:between(0.0,inf)
    is_stale_file/2,         % +File, +FreshnessLifetime:between(0.0,inf)
    latest_file/2,           % +Files, -LatestFile
    thread_file/2,           % +File, -ThreadFile
    touch/1,                 % +File
    root_prefix/1            % ?Prefix
  ]
).
:- reexport(library(filesex)).

/** <module> File extensions

Extensions to the file operations in the standard SWI-Prolog libraries.

@author Wouter Beek
@version 2015/07-2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(filesex)).
:- use_module(library(http/http_receive)).
:- use_module(library(lists)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/dir_ext)).
:- use_module(library(os/os_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(process)).





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



%! file_age(+File, -Age:float) is det.

file_age(File, Age) :-
  time_file(File, LastModified),
  get_time(Now),
  Age is Now - LastModified.



%! file_extensions(+File, -Extensions:list(atom)) is det.

file_extensions(File, Exts) :-
  file_paths(File, Paths),
  last(Paths, Path),
  atomic_list_concat([_|Exts], ., Path).



%! file_name(+Metadata, -Name) is det.

file_name(M, Name) :-
  % The Content-Disposition HTTP header may contain the intended file name.
  http_header(M, 'llo:content-disposition', ContentDisposition),
  get_dict(ContentDisposition, filename, Base0),

  % An archive may add an entry path to the archive file path.
  (   get_dict(M, 'llo:archive_entry', ArchiveEntry),
      archive_entry_path(ArchiveEntry, ArchiveEntryPath)
  ->  directory_file_path(Base0, ArchiveEntryPath, Base)
  ;   Base = Base0
  ),

  % The Content-Type HTTP header may hint at a file extension.
  % @tbd Implement common file extensions for MIME types.
  %(   http_header(M, 'content-type', ContentType)
  %    mime_ext(ContentType.type, ContentType.subtype, Ext),
  %    \+ file_name_extension(_, Ext, Base)
  %->  file_name_extension(Base, Ext, Name)
  %;   Name = Base
  %),
  Name = Base.



%! file_paths(+File, -Paths:list(atom)) is det.

file_paths(File, Paths) :-
  atomic_list_concat(Paths, /, File).



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



%! thread_file(+File, -ThreadFile) is det.
% Returns a thread-specific file name based on the given file name.

thread_file(Base, File) :-
  thread_name(Thread),
  file_name_extension(Base, Thread, File).



%! touch(+File) is det.

touch(File) :-
  setup_call_cleanup(open(File, write, Write), true, close(Write)).
