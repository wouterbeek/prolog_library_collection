:- module(
  file_ext,
  [
    create_file/1, % +File:atom
    create_file_directory/1, % +File:atom
    dateTime_file/2, % +Directory, -File
    dateTime_file/3, % +Directory:atom
                     % ?Extension:atom
                     % -File:atom
    file_age/2, % +File:atom
                % -Age:float
    is_fresh_file/2, % +File:atom
                     % +FreshnessLifetime:between(0.0,inf)
    is_stale_file/2, % +File:atom
                     % +FreshnessLifetime:between(0.0,inf)
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
@version 2015/07-2015/08
*/

:- use_module(library(apply)).
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
  create_directory(Dir).



%! dateTime_file(+Directory:compound, -File:atom) is det.

dateTime_file(Dir, File):-
  dateTime_file(Dir, _, File).

%! dateTime_file(+Directory:compound, ?Extension:atom, -File:atom) is det.

dateTime_file(Spec, Ext, Path):-
  date_directory(Spec, Dir),
  get_time(TimeStamp),
  format_time(atom(Hour), '%H', TimeStamp),
  format_time(atom(Minute), '%M', TimeStamp),
  format_time(atom(Second), '%S', TimeStamp),
  format(atom(Base), '~a_~a_~a', [Hour,Minute,Second]),
  (   var(Ext)
  ->  Local = Base
  ;   file_name_extension(Base, Ext, Local)
  ),
  directory_file_path(Dir, Local, Path),
  create_file(Path).



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
