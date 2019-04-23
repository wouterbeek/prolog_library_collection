:- module(
  archive_ext,
  [
    archive_call/2,       % +In, :Goal_1
    archive_extension/1,  % ?Extension
    archive_media_type/1, % ?MediaType
    archive_open/2,       % +In, -Archive
    archive_stream/2      % +In1, -In2
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2017-2019
*/

:- use_module(library(yall)).

:- use_module(library(media_type)).

:- meta_predicate
    archive_call(+, 1).





%! archive_call(+In:stream, :Goal_1) is det.
%
% The following call is made:
%
% ```
% call(Goal_1, Arch)
% ```

archive_call(In, Goal_1) :-
  setup_call_cleanup(
    archive_open(In, Arch),
    call(Goal_1, Arch),
    archive_close(Arch)
  ).



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



%! archive_open(+In:stream, -Archive:blob) is det.

archive_open(In, Archive) :-
  findall(format(Format), archive_format(Format), Formats),
  archive_open(In, Archive, Formats).



%! archive_stream(+In1:stream, -In2:stream) is nondet.

archive_stream(In1, In2) :-
  archive_call(In1, {In2}/[Arch]>>archive_data_stream(Arch, In2, [])).
