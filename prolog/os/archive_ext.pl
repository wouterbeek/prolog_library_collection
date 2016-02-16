:- module(
  archive_ext,
  [
    archive_entry_path/2, % +ArchiveEntry, -Path
    archive_extract/1,    % +Source
    archive_extract/2,    % +Source, ?Directory
    archive_info/1,       % +Source
    archive_info/2,       % +Source, +Opts
    call_on_archive/2,    % +Source, :Goal_2
    call_on_archive/3     % +Source, :Goal_2, +Opts
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09-2015/11, 2016/01
*/

:- use_module(library(debug_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_info)).
:- use_module(library(os/open_any2)).
:- use_module(library(yall)).

:- meta_predicate
    call_on_archive(+, 2),
    call_on_archive(+, 2, +),
    call_on_archive0(+, 2, +),
    call_on_archive_entry0(+, 2, +, +, +).

:- predicate_options(archive_info/2, 2, [
     indent(+nonneg),
     pass_to(call_on_archive/3, 3)
   ]).
:- predicate_options(call_on_archive/3, 3, [
     archive_entry(+atom),
     pass_to(call_on_archive_entry0/5, 3),
     pass_to(call_on_archive0/3, 3)
   ]).
:- predicate_options(call_on_archive0/3, 3, [
     pass_to(open_any2/5, 5),
     pass_to(archive_open/3, 3)
   ]).
:- predicate_options(call_on_archive_entry0/5, 3, [
     pass_to(archive_data_stream/3, 3)
   ]).





%! archive_entry_path(+ArchiveEntry, -Path) is det.

% The raw archive entry's path is the empty path.
archive_entry_path([H], '') :-
  is_unarchived(H), !.
% A non-raw archive entry: add its name to the path.
archive_entry_path([H|T1], EntryPath2) :-
  archive_entry_path(T1, EntryPath1),
  directory_file_path(EntryPath1, H.'llo:name', EntryPath2).

% Succeeds if dictionary M describes a leaf node in a compression tree.
% A leaf node in a compression tree describes an unarchived or raw file.
is_unarchived(M) :-
  M.'llo:name' == "data",
  M.'llo:format' == "raw", !.



%! archive_extract(+Source) is det.

archive_extract(Source) :-
  archive_extract(Source, _).


%! archive_extract(+Source, ?Directory) is det.
% Extracts the given file into the given directory.
%
% In case no directory is given, the directory of the given source is used.
%
% @throws instantiation_error When File is a variable.
% @throws type_error When `Source` is neither an absolute file name nor a URL.

archive_extract(Source, Dir) :-
  (var(Dir) -> file_directory_name(Source, Dir) ; true),
  call_on_archive(Source, archive_extract_entry(Dir)).

archive_extract_entry(Dir, M, Read) :-
  directory_file_path(Dir, M.'llo:ArchiveEntry', Path),
  setup_call_cleanup(
    open_any2(Path, write, Write, Close_0),
    copy_stream_data(Read, Write),
    close_any2(Close_0)
  ).



%! archive_info(+Source) is det.
%! archive_info(+Source, +Opts) is det.
% Writes archive information for the given file or URL to current input.
%
% # Example
%
% ```prolog
% ?- absolute_file_name(data('abcde.tar.gz'), File, [access(read)]),
%    archive_info(File).
% ab.tar.gz
%   filetype(file)
%   mtime(1402126051.0)
%   size(128)
%   format(posix ustar format)
%   a.txt
%     filetype(file)
%     mtime(1402126033.0)
%     size(2)
%     format(posix ustar format)
%   b.txt
%     filetype(file)
%     mtime(1402126038.0)
%     size(2)
%     format(posix ustar format)
% cd.tar.gz
%   filetype(file)
%   mtime(1402126098.0)
%   size(128)
%   format(posix ustar format)
%   d.txt
%     filetype(file)
%     mtime(1402126074.0)
%     size(2)
%     format(posix ustar format)
%   c.txt
%     filetype(file)
%     mtime(1402126067.0)
%     size(2)
%     format(posix ustar format)
% e.txt
%   filetype(file)
%   mtime(1402126131.0)
%   size(2)
%   format(posix ustar format)
% File = '.../data/abcde.tar.gz'.
% ```
%
% The following options are supported:
%   * indent(+nonneg)
%     Default is 0.

archive_info(Source) :-
  archive_info(Source, []).

archive_info(Source, Opts) :-
  option(indent(I), Opts, 0),
  call_on_archive(Source, [M,_Arch]>>print_dict(M, I), Opts).



%! call_on_archive(+Source, :Goal_2) is det.
%! call_on_archive(+Source, :Goal_2, +Opts) is det.
% Goal_2 called on a metadata dictionary and a read stream (in that order).
%
% @throws existence_error if an HTTP request returns an error code.

call_on_archive(Source, Goal_2) :-
  call_on_archive(Source, Goal_2, []).

call_on_archive(Source, Goal_2, Opts) :-
  option(archive_entry(Entry), Opts, _),
  call_on_archive0(Source, call_on_archive_entry0(Entry, Goal_2, Opts), Opts).

call_on_archive0(Source, Goal_2, Opts1) :-
  % Archive options that cannot be overridden.
  merge_options([close_parent(false)], Opts1, Opts2),
  % Archive options that can be overridden.
  merge_options(Opts2, [filter(all),format(all),format(raw)], Opts3),
  setup_call_cleanup(
    open_any2(Source, read, Read, Close_0, [metadata(M)|Opts1]),
    setup_call_cleanup(
      archive_open(Read, Arch, Opts3),
      call(Goal_2, M, Arch),
      archive_close(Arch)
    ),
    close_any2(Close_0)
  ).

% Semi-deterministic if an archive entry name is given.
call_on_archive_entry0(Entry, Goal_2, Opts, M, Arch) :-
  nonvar(Entry), !,
  call_on_archive_entry_nondet0(Entry, Goal_2, Opts, M, Arch), !.
% Non-deterministic if no archive entry name is given.
call_on_archive_entry0(Entry, Goal_2, Opts, M, Arch) :-
  call_on_archive_entry_nondet0(Entry, Goal_2, Opts, M, Arch).

call_on_archive_entry_nondet0(Entry, Goal_2, Opts1, M1, Arch) :-
  merge_options([meta_data(MEntry1)], Opts1, Opts2),
  archive_data_stream(Arch, Read, Opts2),
  maplist(jsonld_metadata, MEntry1, MEntry2),
  (   MEntry2 = [MEntryH|_],
      atom_string(Entry, MEntryH.'llo:name')
  ->  M2 = M1.put(_{'llo:ArchiveEntry': MEntry2}),
      call_cleanup(call(Goal_2, M2, Read), close(Read))
  ;   close(Read),
      fail
  ).

jsonld_metadata(
  archive_meta_data{
    filetype: Filetype1,
    filters: Filters1,
    format: Format1,
    mtime: Mtime,
    name: Name1,
    size: Size
  },
  _{
    '@type': 'llo:ArchiveEntry',
    'llo:filetype': Filetype2,
    'llo:filters': Filters2,
    'llo:format': Format2,
    'llo:mtime': Mtime,
    'llo:name': Name2,
    'llo:size': Size
  }
) :-
  maplist(
    atom_string,
    [Filetype1,Format1,Name1|Filters1],
    [Filetype2,Format2,Name2|Filters2]
  ).
