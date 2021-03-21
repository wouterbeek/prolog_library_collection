:- module(
  archive_ext,
  [
    archive_call/2,       % +In, :Goal_2
    archive_extension/1,  % ?Extension
    archive_media_type/1, % ?MediaType
    archive_open/2,       % +In, -Archive
    archive_stream/2      % +In1, -In2
  ]
).
:- reexport(library(archive)).

/** <module> Extended support for handling archives

This module extends the support for archives in the SWI-Prolog
standard library.

*/

:- use_module(library(yall)).

:- use_module(library(media_type)).

:- meta_predicate
    archive_call(+, 2).





%! archive_call(+In:istream, :Goal_2) is multi.
%
% Calls `Goal_2' on a decompressed input stream that is present in the
% encoded input stream `In' and the metadata object `Metas'.
%
% Supports non-deterministically iterating over all archive members.
%
% Uses archive_open/2 to automatically process all supporter archive
% filters and formats.

archive_call(In1, Goal_2) :-
  setup_call_cleanup(
    archive_open(In1, Arch),
    (
      archive_data_stream(Arch, In2, [meta_data(Metas)]),
      call(Goal_2, In2, Metas)
    ),
    archive_close(Arch)
  ).



%! archive_extension(+Extension:atom) is semidet.
%! archive_extension(-Extension:atom) is multi.
%
% Uses the Media Type library in order to determine whether a given
% `Extension' is commonly used to denote an archive file.
%
% Can also be used to enumerate such known file name extensions.

archive_extension(Ext) :-
  var(Ext), !,
  archive_media_type(MediaType),
  media_type_extension(MediaType, Ext).
archive_extension(Ext) :-
  media_type_extension(MediaType, Ext),
  archive_media_type(MediaType).



%! archive_filter(+Filter:atom) is semidet.
%! archive_filter(-Filter:atom) is multi.
%
% Succeeds for all and only filter-denoting atoms that are supported
% by the archive library.

archive_filter(Filter) :-
  archive_media_type_filter_(_, Filter).
archive_filter(compress).
archive_filter(grzip).
archive_filter(lrzip).
archive_filter(lzip).
archive_filter(lzma).
archive_filter(lzop).
archive_filter(rpm).
archive_filter(uu).

archive_media_type_filter_(media(application/'x-bzip2',[]), bzip2).
archive_media_type_filter_(media(application/gzip,[]), gzip).
archive_media_type_filter_(media(application/'x-xz',[]), xz).



%! archive_format(+Format:atom) is semidet.
%! archive_format(-Format:atom) is multi.
%
% Succeeds for all and only format-denoting atoms that are supported
% by the archive library.
%
% This predicate purposefully skips the `mtree' archive format,
% becasue its use results in many false positives in real-world use
% cases.  `mtree` is a plain text format, and libarchive someitmes
% considers a regular (non-archive) text file to be of this format.
% Also, `mtree` archives are rarely used in practice, so the loss of
% excluding this format is not that big.

archive_format(Format) :-
  archive_media_type_format_(_, Format).
archive_format(ar).
archive_format(empty).
archive_format(iso9660).
archive_format(raw).
% eXtensible ARchive
archive_format(xar).



%! archive_media_type(+MediaType:media_type) is semidet.
%! archive_media_type(-MediaType:media_type) is multi.
%
% Succeeds for all and only those Media Types that denote an archive
% format.  These are recorded in the Media Type library.

archive_media_type(MediaType) :-
  ground(MediaType), !,
  (   archive_media_type_filter_(MediaType, _)
  ;   archive_media_type_format_(MediaType, _)
  ), !.
archive_media_type(MediaType) :-
  distinct(
    MediaType,
    (   archive_media_type_filter_(MediaType, _)
    ;   archive_media_type_format_(MediaType, _)
    )
  ).

archive_media_type_format_(media(application/'x-7z-compressed',[]), '7zip').
archive_media_type_format_(media(application/'vnd.ms-cab-compressed',[]), cab).
archive_media_type_format_(media(application/'x-cpio',[]), cpio).
archive_media_type_format_(media(application/'x-tar',[]), gnutar).
archive_media_type_format_(media(application/'x-lzh-compressed',[]), lha).
archive_media_type_format_(media(application/'vnd.rar',[]), rar).
archive_media_type_format_(media(application/'x-tar',[]), tar).
archive_media_type_format_(media(application/zip,[]), zip).



%! archive_open(+In:istream, -Archive:blob) is det.
%
% Tries to open an archive of any of the supported formats, using any
% of the supported filters, from the input stream `In`.

archive_open(In, Archive) :-
  findall(format(Format), archive_format(Format), Formats),
  archive_open(In, Archive, Formats).



%! archive_stream(+In1:istream, -In2:istream) is det.

archive_stream(In1, In2) :-
  setup_call_cleanup(
    archive_open(In1, Archive),
    archive_data_stream(Archive, In2, []),
    archive_close(Archive)
  ).
