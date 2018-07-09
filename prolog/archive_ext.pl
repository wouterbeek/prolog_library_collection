:- module(
  archive_ext,
  [
    archive_extension/1,  % ?Extension
    archive_media_type/1, % ?MediaType
    archive_open/2        % +In, -Archive
  ]
).

/** <module> Archive extensions

@author Wouter Beek
@version 2017-2018
*/

:- reexport(library(archive)).
:- use_module(library(media_type)).





%! archive_extension(+Extension:atom) is semidet.
%! archive_extension(-Extension:atom) is multi.

archive_extension(Ext) :-
  var(Ext), !,
  archive_media_type(MediaType),
  media_type_extension(MediaType, Ext).
archive_extension(Ext) :-
  media_type_extension(MediaType, Ext),
  archive_media_type(MediaType).



%! archive_filter(+Filter:atom) is semidet.
%! archive_filter(-Filter:atom) is multi.

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

archive_format(Format) :-
  archive_media_type_format_(_, Format).
archive_format(ar).
archive_format(empty).
archive_format(iso9660).
% The `mtree' format is not supported, because archives in that format
% are regular text files and result in false positives.
% archive_format(mtree).
archive_format(raw).
% eXtensible ARchive
archive_format(xar).



%! archive_media_type(+MediaType:compound) is semidet.
%! archive_media_type(-MediaType:compound) is multi.

archive_media_type(MediaType) :-
  archive_media_type_filter_(MediaType, _).
archive_media_type(MediaType) :-
  archive_media_type_format_(MediaType, _).

archive_media_type_format_(media(application/'x-7z-compressed',[]), '7zip').
archive_media_type_format_(media(application/'vnd.ms-cab-compressed',[]), cab).
archive_media_type_format_(media(application/'x-cpio',[]), cpio).
archive_media_type_format_(media(application/'x-tar',[]), gnutar).
archive_media_type_format_(media(application/'x-lzh-compressed',[]), lha).
archive_media_type_format_(media(application/'vnd.rar',[]), rar).
archive_media_type_format_(media(application/'x-tar',[]), tar).
archive_media_type_format_(media(application/zip,[]), zip).



%! archive_open(+In:stream, -Archive:blob) is det.

archive_open(In, Archive) :-
  findall(format(Format), archive_format(Format), Formats),
  archive_open(In, Archive, Formats).
