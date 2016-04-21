:- module(
  archive_ext,
  [
    archive_entry_path/2, % +ArchiveEntry, -Path
    archive_extract/1,    % +Source
    archive_extract/2,    % +Source, ?Directory
    archive_info/1,       % +Source
    archive_info/2        % +Source, +Opts
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2015/09-2015/11, 2016/01-2016/04
*/

:- use_module(library(debug_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(os/open_any2)).
:- use_module(library(print_ext)).
:- use_module(library(yall)).





%! archive_entry_path(+ArchiveEntry, -Path) is det.

% The raw archive entry's path is the empty path.
archive_entry_path([H], '') :-
  is_unarchived0(H), !.
% A non-raw archive entry: add its name to the path.
archive_entry_path([H|T1], EntryPath2) :-
  archive_entry_path(T1, EntryPath1),
  directory_file_path(EntryPath1, H.'llo:name', EntryPath2).


% Succeeds if dictionary M describes a leaf node in a compression tree.
% A leaf node in a compression tree describes an unarchived or raw file.
is_unarchived0(M) :-
  M.'llo:name' == "data",
  M.'llo:format' == "raw", !.



%! archive_extract(+Source) is det.
%! archive_extract(+Source, +Dir) is det.
% Extracts the given file into the given directory.
%
% In case no directory is given, the directory of the given source is used.
%
% @throws instantiation_error When File is a variable.
% @throws type_error When `Source` is neither an absolute file name nor a URL.

archive_extract(Source) :-
  file_directory_name(Source, Dir),
  archive_extract(Source, Dir).


archive_extract(Source, Dir) :-
  call_on_stream(Source, [In,M,_]>>copy_stream_data0(Dir, In, M)).

copy_stream_data0(Dir, In, M) :-
  directory_file_path(Dir, M.'llo:archive_entry', Sink),
  call_to_stream(Sink, [Out,_,_]>>copy_stream_data(In, Out)).



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
  call_on_stream(Source, [_,M,_]>>print_dict(M, Opts), Opts).
