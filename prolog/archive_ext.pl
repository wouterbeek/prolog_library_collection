:- module(
  archive_ext,
  [
    archive_format/1, % ?Format
    archive_open/2,   % +In, -Archive
    archive_path/3    % +File, -Path, +Options
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2017/07-2017/12
*/





%! archive_format(+Format:atom) is semidet.
%! archive_format(-Format:atom) is multi.
%
% Format are the names of supported archive formats.  The `mtree'
% format is not supported, because archives in that format are regular
% text files and result in false positives.

archive_format('7zip').
archive_format(ar).
archive_format(cab).
archive_format(cpio).
archive_format(empty).
archive_format(gnutar).
archive_format(iso9660).
archive_format(lha).
%archive_format(mtree).
archive_format(rar).
archive_format(raw).
archive_format(tar).
archive_format(xar).
archive_format(zip).



%! archive_open(+In:stream, -Archive:blob) is det.

archive_open(In, Archive) :-
  findall(format(Format), archive_format(Format), Formats),
  archive_open(In, Archive, [filter(all)|Formats]).



%! archive_path(+File:atom, -Path:list(dict), +Options:list(compound)) is nondet.

archive_path(File, Path, Options) :-
  setup_call_cleanup(
    open(File, read, In, Options),
    setup_call_cleanup(
      archive_open(In, Archive, Options),
      setup_call_cleanup(
        archive_data_stream(Archive, In, [meta_data(Path)]),
        true,
        close(In)
      ),
      archive_close(Archive)
    ),
    close(In)
  ).
