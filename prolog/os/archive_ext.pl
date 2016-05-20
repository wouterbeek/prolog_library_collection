:- module(
  archive_ext,
  [
    archive_extract2/2,       % +File, -File2
    archive_extract2/3,       % +File, +EntryName, -File2
    archive_file_extension/1, % ?Ext
    file_entry_path/3,        % +File1, +MEntryPath, -File2
    is_archive_file_name/1,   % +File
    print_archive/1,          % +Source
    print_archive/2           % +Source, +Opts
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09-2015/11, 2016/01-2016/05
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(print_ext)).
:- use_module(library(yall)).





%! archive_extract2(+File1, +File2) is nondet.
%! archive_extract2(+File1, +EntryName, +File2) is nondet.
%
% Extracts the given file into the given directory.
%
% In case no directory is given, the directory of the given source is
% used.

archive_extract2(File1, File2) :-
  archive_extract0(File1, File2, []).


archive_extract2(File1, EntryName, File2) :-
  archive_extract0(File1, File2, [entry_name(EntryName)]).

archive_extract0(File1, File2, Opts) :-
  call_on_stream(
    File1,
    {File1,File2}/[In,M,M]>>copy_stream_data0(File1, File2, In, M),
    Opts
  ).

copy_stream_data0(File1, File2, In, M) :-
  file_entry_path(File1, M.'llo:entry_path', File2),
  create_file_directory(File2),
  call_to_stream(File2, {In}/[Out,M,M]>>copy_stream_data(In, Out)).



%! archive_file_extension(+Ext) is semidet.
%! archive_file_extension(-Ext) is multi.
%
% Often occurring file extensions for archvies.

archive_file_extension(cab).
archive_file_extension(rar).
archive_file_extension(tar).
archive_file_extension(xar).
archive_file_extension(zip).



%! is_archive_file_name(+File) is semidet.
%
% Succeeds if File is a common way of naming an archive file.

is_archive_file_name(File) :-
  file_extension(File, Ext),
  archive_file_extension(Ext).



%! file_entry_path(+File1, +MEntryPath, -File2) is det.

file_entry_path(File1, MEntryPath0, File2) :-
  exclude(is_unarchived, MEntryPath0, MEntryPath),
  maplist(file_entry_path_comp, MEntryPath, Comps),
  atomic_list_concat(Comps, /, Path),
  relative_file_name(File2, File1, Path).

% Succeeds if dictionary M describes a leaf node in a compression tree.
% A leaf node in a compression tree describes an unarchived or raw file.
is_unarchived(M) :-
  M.'llo:name' == "data",
  M.'llo:format' == "raw", !.

file_entry_path_comp(MEntry, MEntry.'llo:name').



%! print_archive(+Source) is det.
%! print_archive(+Source, +Opts) is det.
%
% Writes archive information for the given file or IRI to current
% input.
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

print_archive(Source) :-
  print_archive(Source, []).


print_archive(Source, Opts) :-
  call_on_stream(
    Source,
    {Opts}/[_,M,M]>>print_entry_path(M, Opts),
    Opts
  ).


print_entry_path(M, Opts) :-
  maplist({Opts}/[X]>>print_dict(X, Opts), M.'llo:entry_path').
