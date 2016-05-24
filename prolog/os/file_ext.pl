:- module(
  file_ext,
  [
    create_file/1,           % +File
    create_file_directory/1, % +File
    file_age/2,              % +File, -Age:float
    file_change_extension/3, % +File1, +Ext, File2
    file_extension/2,        % +File, -Ext
    file_extensions/2,       % +File, -Exts
    file_paths/2,            % +File, -Paths
    file_size/2,             % +File, -Size
    is_fresh_age/2,          % +Age:between(0.0,inf), +FreshnessLifetime:between(0.0,inf)
    is_fresh_file/2,         % +File, +FreshnessLifetime:between(0.0,inf)
    is_stale_age/2,          % +Age:between(0.0,inf), +FreshnessLifetime:between(0.0,inf)
    is_stale_file/2,         % +File, +FreshnessLifetime:between(0.0,inf)
    latest_file/2,           % +Files, -LatestFile
    metadata_file_name/2,    % +M, -File
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
@version 2015/07-2015/11, 2016/01-2016/03, 2016/05
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(filesex)).
:- use_module(library(http/http_ext)).
:- use_module(library(lists)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/dir_ext)).
:- use_module(library(os/os_ext)).
:- use_module(library(os/thread_ext)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(uuid_ext)).





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



%! file_change_extension(+From, +Ext, -To) is det.

file_change_extension(From, Ext, To) :-
  file_name_extension(Base, _, From),
  file_name_extension(Base, Ext, To).



%! file_extension(+File, -Ext) is semidet.

file_extension(File, Ext) :-
  file_name_extension(_, Ext, File).



%! file_extensions(+File, -Exts) is det.

file_extensions(File, Exts) :-
  file_paths(File, Paths),
  last(Paths, Path),
  atomic_list_concat([_|Exts], ., Path).



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



%! metadata_file_name(+M, -Name) is det.
%
% Extract a file name from source metadata.

metadata_file_name(M, Name) :-
  % The Content-Disposition HTTP header may contain the intended file name.
  http_header(M, 'llo:content-disposition', ContentDisposition),
  get_dict(ContentDisposition, filename, Base0),

  % An archive may add an entry path to the archive file path.
  (   get_dict(M, 'llo:entry_path', MEntryPath),
      file_entry_path(MEntryPath, Path)
  ->  directory_file_path(Base0, Path, Base)
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
  uuid_no_hyphen(Base),
  thread_file(Base, File).


thread_file(Base, File) :-
  thread_name(Thread),
  file_name_extension(Base, Thread, File).



%! touch(+File) is det.

touch(File) :-
  setup_call_cleanup(open(File, write, Write), true, close(Write)).
