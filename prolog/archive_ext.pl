:- module(
  archive_ext,
  [
    archive_extension/1,  % ?Ext
    archive_media_type/1, % ?MediaType
    archive_open/2,       % +In, -Archive
    archive_path/3        % +File, -Path, +Options
  ]
).

/** <module> Archive extensions

@author Wouter Beek
@version 2017-2018
*/

:- reexport(library(archive)).
:- use_module(library(media_type)).





%! archive_extension(+Ext:atom) is semidet.
%! archive_extension(-Ext:atom) is multi.

archive_extension(Ext) :-
  var(Ext), !,
  archive_media_type_format_(MediaType, _),
  media_type_extension(MediaType, Ext).
archive_extension(Ext) :-
  media_type_extension(MediaType, Ext),
  archive_media_type_format_(MediaType, _).



%! archive_format(+Fromat:atom) is semidet.
%! archive_format(-Fromat:atom) is multi.

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
