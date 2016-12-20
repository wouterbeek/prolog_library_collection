:- module(
  archive_ext,
  [
    archive_file_extension/1, % ?Ext
    archive_format/1,         % ?Format
    archive_format/2,         % ?Format, ?Supported
    archive_path/2,           % +Source, -InPath
    file_entry_path/2,        % +MetaPath, -File2
    file_entry_path/3,        % +File1, +MetaPath, -File2
    is_archive_file/1,        % +File
    path_entry_name/2         % +Path, -EntryName
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(archive)).
:- use_module(library(debug)).
:- use_module(library(dict_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(lists)).
:- use_module(library(os/file_ext)).
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
%! archive_format(-Format, -Supported) is multi.
%
% Some archive formats are not Supported because they often get
% mistaken for regular text files.

archive_format(Format) :-
  archive_format(Format, _).


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

    

%! archive_path(+Source, -InPath) is nondet.

archive_path(Source, InPath) :-
  call_on_stream(Source, archive_path0(InPath)).

archive_path0(InPath, _, InPath, InPath).



%! is_archive_file(+File) is semidet.
%
% Succeeds if File is a common way of naming an archive file.

is_archive_file(File) :-
  file_extensions(File, Exts),
  once((
    member(Ext, Exts),
    archive_file_extension(Ext)
  )).



%! file_entry_path(+Path, -File) is det.
%! file_entry_path(+File1, +Path, -File2) is det.

file_entry_path(Path, File) :-
  file_entry_path('', Path, File).


file_entry_path(File1, Path1, File2) :-
  exclude(is_unarchived0, Path1, Path2),
  maplist(file_entry_path_comp0, Path2, Comps),
  atomic_list_concat(Comps, /, Path2),
  relative_file_name(File2, File1, Path2).

% Succeeds if dictionary M describes a leaf node in a compression
% tree.  A leaf node in a compression tree describes an unarchived or
% raw file.
is_unarchived0(Entry) :-
  Entry.name == "data",
  Entry.format == "raw", !.

file_entry_path_comp0(Entry, Entry.name).



%! path_entry_name(+Path, -EntryName) is det.
%
% EntryName is the most descriptive entry name associated with the
% given Path.  This means that it is either the entry name that is
% given in archive metadata (if available), or the name `data`.

path_entry_name(Path, EntryName) :-
  dicts_get(name, Path, EntryName),
  EntryName \== data, !.
path_entry_name(Path, EntryName) :-
  dicts_getchk(name, Path, EntryName).
