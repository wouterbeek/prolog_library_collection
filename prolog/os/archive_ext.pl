:- module(
  archive_ext,
  [
    archive_file_extension/1, % ?Ext
    archive_format/1,         % ?Format
    archive_format/2,         % ?Format, ?Supported
    file_entry_path/2,        % +MetaPath, -File2
    file_entry_path/3,        % +File1, +MetaPath, -File2
    is_archive_file/1,        % +File
    print_archive/1,          % +Source
    print_archive/2           % +Source, +Opts
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09-2015/11, 2016/01-2016/05, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(debug_ext)).
:- use_module(library(fileutils)).
:- use_module(library(http/http_ext)).
:- use_module(library(os/io)).
:- use_module(library(print_ext)).
:- use_module(library(true)).
:- use_module(library(yall)).





%! archive_file_extension(+Ext) is semidet.
%! archive_file_extension(-Ext) is multi.
%
% Often occurring file extensions for archvies.

archive_file_extension(cab).
archive_file_extension(rar).
archive_file_extension(tar).
archive_file_extension(xar).
archive_file_extension(zip).



%! archive_format(-Format) is multi.

archive_format(Format) :-
  archive_format(Format, _).


%! archive_format(-Format, -Supported) is multi.
%
% Some archive formats are not Supported because they often get
% mistaken for regular text files.

archive_format('7zip',  true ).
archive_format(ar,      true ).
archive_format(cab,     true ).
archive_format(cpio,    true ).
archive_format(empty,   true ).
archive_format(gnutar,  true ).
archive_format(iso9660, true ).
archive_format(lha,     true ).
archive_format(mtree,   false).
archive_format(rar,     true ).
archive_format(raw,     true ).
archive_format(tar,     true ).
archive_format(xar,     true ).
archive_format(zip,     true ).

    

%! is_archive_file(+File) is semidet.
%
% Succeeds if File is a common way of naming an archive file.

is_archive_file(File) :-
  file_extension(File, Ext),
  archive_file_extension(Ext).



%! file_entry_path(+MetaPath, -File) is det.
%! file_entry_path(+File1, +MetaPath, -File2) is det.

file_entry_path(MetaPath, File) :-
  file_entry_path('', MetaPath, File).


file_entry_path(File1, MetaPath0, File2) :-
  exclude(is_unarchived0, MetaPath0, MetaPath),
  maplist(file_entry_path_comp0, MetaPath, Comps),
  atomic_list_concat(Comps, /, Path),
  relative_file_name(File2, File1, Path).


% Succeeds if dictionary M describes a leaf node in a compression
% tree.  A leaf node in a compression tree describes an unarchived or
% raw file.
is_unarchived0(MetaEntry) :-
  MetaEntry.name == "data",
  MetaEntry.format == "raw", !.


file_entry_path_comp0(MetaEntry, MetaEntry.name).



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

print_archive(Source) :-
  print_archive(Source, []).


print_archive(Source, Opts) :-
  call_on_stream(Source, [_,Meta,Meta]>>print_dict(Meta), Opts).
