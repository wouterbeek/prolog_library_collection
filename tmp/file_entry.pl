:- module(
  file_entry,
  [
    directory_file_entry/2, % +Dir, -FileEntry
    file_entry_file/3,      % +FileEntry, +Ext, -File
    file_entry_extension/2  % +FileEntry, ?Ext
  ]
).

/** <module> File entry

Support for archive entries inside data files.  For many purposes we
want to abstract away from whether a data file is archive or not;
compressed or not.

@author Wouter Beek
@version 2016/05
*/

:- use_module(library(fileutils)).
:- use_module(library(os/archive_ext)).
:- use_module(library(os/directory_ext)).
:- use_module(library(os/io)).





%! directory_file_entry(+Dir, -FileEntry) is nondet.

directory_file_entry(Dir, file_entry(File,Entry)) :-
  directory_file_recursive(Dir, File),
  call_on_stream(File, true, [entry_name(Entry)]).



%! file_entry_file(+FileEntry, +Ext, -File) is det.

file_entry_file(file_entry(File1,Entry), Ext, File2) :-
  relative_file_name(File0, File1, Entry),
  file_change_extension(File0, Ext, File2),
  create_file_directory(File2).



%! file_entry_extension(+FileEntry, +Ext) is semidet.
%! file_entry_extension(+FileEntry, -Ext) is semidet.
%
% Heuristically determine the Ext of data that is stored in
% FileEntry.

file_entry_extension(file_entry(File,Entry), Ext) :-
  is_archive_file(File), !,
  file_extension(Entry, Ext).
file_entry_extension(file_entry(File,_), Ext) :-
  file_extension(File, Ext).
