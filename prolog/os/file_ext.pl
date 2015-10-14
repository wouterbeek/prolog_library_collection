:- module(
  file_ext,
  [
    create_file/1, % +File:atom
    create_file_directory/1, % +File:atom

    file_age/2, % +File:atom
                % -Age:float
    is_fresh_age/2, % +Age:between(0.0,inf)
                    % +FreshnessLifetime:between(0.0,inf)
    is_fresh_file/2, % +File:atom
                     % +FreshnessLifetime:between(0.0,inf)
    is_stale_age/2, % +Age:between(0.0,inf)
                    % +FreshnessLifetime:between(0.0,inf)
    is_stale_file/2, % +File:atom
                     % +FreshnessLifetime:between(0.0,inf)
    latest_file/2, % +Files:list(atom)
                   % -LatestFile:atom
    thread_file/2, % +File:atom
                   % -ThreadFile:atom
    touch/1, % +File:atom
    root_prefix/1, % ?Prefix:atom
    write_stream_to_file/2 % +Write:stream
                           % +File:atom
  ]
).
:- reexport(library(filesex)).

/** <module> File extensions

Extensions to the file operations in the standard SWI-Prolog libraries.

@author Wouter Beek
@version 2015/07-2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(os/dir_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/os_ext)).
:- use_module(library(process)).





%! create_file(+File:atom) is det.
% @throws type_error

create_file(File):-
  exists_file(File), !.
create_file(File):-
  is_absolute_file_name(File), !,
  touch(File).
create_file(File):-
  type_error(absolute_file_name, File).



%! create_file_directory(+Path:atom) is det.
% Ensures that the directory structure for the given file exists.

create_file_directory(Path):-
  (exists_directory(Path) -> Dir = Path ; directory_file_path(Dir, _, Path)),
  make_directory_path(Dir).



%! file_age(+File:atom, -Age:float) is det.

file_age(File, Age):-
  time_file(File, LastModified),
  get_time(Now),
  Age is Now - LastModified.



%! is_fresh_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_fresh_age(_, inf):- !.
is_fresh_age(Age, FreshnessLifetime):-
  Age =< FreshnessLifetime.



%! is_fresh_file(+File:atom, +FreshnessLifetime:between(0.0,inf)) is semidet.

is_fresh_file(File, FreshnessLifetime):-
  file_age(File, Age),
  is_fresh_age(Age, FreshnessLifetime).



%! is_stale_age(
%!   +Age:between(0.0,inf),
%!   +FreshnessLifetime:between(0.0,inf)
%! ) is semidet.

is_stale_age(_, inf):- !, fail.
is_stale_age(Age, FreshnessLifetime):-
  Age > FreshnessLifetime.



%! is_stale_file(+File:atom, +FreshnessLifetime:between(0.0,inf)) is semidet.

is_stale_file(File, FreshnessLifetime):-
  file_age(File, Age),
  is_stale_age(Age, FreshnessLifetime).



%! latest_file(+Files:list(atom), -Latest:atom) is det.
% Returns the most recently created or altered file from within a list of
% files.

latest_file([H|T], Latest):-
  time_file(H, Time),
  latest_file(T, Time-H, Latest).

latest_file([], _-Latest, Latest).
latest_file([H|T], Time1-File1, Latest):-
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



%! thread_file(+File:atom, -ThreadFile:atom) is det.
% Returns a thread-specific file name based on the given file name.

thread_file(File, ThreadFile):-
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', ThreadName),
  file_name_extension(File, ThreadName, ThreadFile).



%! touch(+File:atom) is det.

touch(File):-
  setup_call_cleanup(open(File, write, Write), true, close(Write)).



%! write_stream_to_file(+Read:stream, +File:atom) is det.

write_stream_to_file(Read, File):-
  setup_call_cleanup(
    open(File, write, Write, [type(binary)]),
    copy_stream_data(Read, Write),
    close(Write)
  ).
