:- module(
  http_download,
  [
    file_download/2, % +Iri, ?File
    file_download/3, % +Iri, ?File, +Opts
    html_download/2, % +Iri, -Dom
    html_download/3, % +Iri, -Dom, +Opts
    json_download/2, % +Iri, -Json
    json_download/3, % +Iri, -Json, +Opts
    xml_download/2,  % +Iri, -Dom
    xml_download/3   % +Iri, -Dom, +Opts
  ]
).

/** <module> HTTP download

Support for downloading files and datastructures over HTTP(S).

---

@author Wouter Beek
@tbd We cannot use library(lambda) because this copies the goal term,
     not returning the DOM argument.  Maybe yall can be used?
@version 2015/07-2015/11, 2016/01
*/

:- use_module(library(atom_ext)).
:- use_module(library(error)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_receive)).
:- use_module(library(http/http_request)).
:- use_module(library(http/json)).
:- use_module(library(os/file_ext)).
:- use_module(library(os/io_ext)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(uri)).

:- predicate_options(file_download/3, 3, [
     freshness_lifetime(+or([between(0.0,inf),oneof([inf])])),
     pass_to(http_get/3, 3)
   ]).
:- predicate_options(html_download/3, 3, [
     pass_to(load_html/3, 3)
   ]).
:- predicate_options(json_download/3, 3, [
     pass_to(http_get/3, 3),
     pass_to(json_read_dict/3, 3)
   ]).
:- predicate_options(xml_download/3, 3, [
     pass_to(load_xml/3, 3)
   ]).





%! file_download(+Iri, ?File) is det.
%! file_download(+Iri, ?File, +Opts) is det.
% Downloads the contents stored at the given URI to either
% the a File with either a given file name or
% a file name that is created based on the URI.
%
% The following options are supported:
%   * freshness_lifetime(+or([between(0.0,inf),oneof([inf])]))
%     Sets whether -- and if so, when -- files that were downloaded
%     in the past are redownloaded and overwritten.
%     Default is `inf`.
%   * Other options are passed to http_get/3.
%
% @throws type_error if Iri is not an absolute URI.

file_download(Iri, File):-
  file_download(Iri, File, []).

% The file was already downloaded in the past.
file_download(Iri, File, Opts):-
  nonvar(File),
  exists_file(File), !,
  (   option(freshness_lifetime(FL), Opts, inf),
      is_fresh_file(File, FL)
  ->  access_file(File, read)
  ;   delete_file(File),
      file_download(Iri, File, Opts)
  ).
% Throw an exception if Iri is not an absolute URI.
file_download(Iri, _, _):-
  \+ uri_is_global(Iri), !,
  type_error(absolute_uri, Iri).
% A file name is given.
file_download(Iri, File, Opts1):-
  iri_normalized(Iri, NormIri),
  md5(NormIri, Hash),
  thread_file(Hash, TmpFile),
  merge_options([metadata(M)], Opts1, Opts2),
  http_get(Iri, write_stream_to_file0(TmpFile), Opts2),
  (   http_header(M, 'content-disposition', ContentDisposition),
      get_dict(ContentDisposition, filename, LocalFile)
  ->  true
  % @tbd Implement common file extensions for MIME types.
  %;   http_header(M, 'content-type', ContentType)
  %->  mime_ext(ContentType.type, ContentType, subtype, Ext),
  %    file_name_extension(Hash, Ext, LocalFile)
  ;   LocalFile = Hash
  ),
  absolute_file_name(LocalFile, File, [access(write)]),
  rename_file(TmpFile, File).

write_stream_to_file0(File, _, Read):-
  write_stream_to_file(Read, File).



%! html_download(+Iri, -Dom) is det.
%! html_download(+Iri, -Dom, +Opts) is det.

html_download(Iri, Dom):-
  html_download(Iri, Dom, []).

html_download(Iri, Dom, Opts):-
  http_get(Iri, load_html0(Dom0, Opts)),
  dom_clean(Dom0, Dom).

load_html0(Dom, Opts, _, Read):-
  load_html(Read, Dom, Opts).


%! dom_clean(+Dom1, -Dom2) is det.
% Clean the given DOM tree in the following two ways:
%   1. Strip all blanks from the beginning and end of all strings.
%   2. Remove all strings that are empty under (1) from the DOM tree.

dom_clean([H1|T1], L2):-
  atom(H1), !,
  % Strip all blanks from strings that appear in the DOM.
  strip_atom(H1, H2),
  % Remove empty strings from the DOM.
  (H2 == '' -> L2 = T2 ; L2 = [H2|T2]),
  dom_clean(T1, T2).
dom_clean([element(N,As,Contents1)|T1], [element(N,As,Contents2)|T2]):- !,
  dom_clean(Contents1, Contents2),
  dom_clean(T1, T2).
dom_clean([], []).



%! json_download(+Iri, -Json) is det.
%! json_download(+Iri, -Json, +Opts) is det.

json_download(Iri, Json):-
  json_download(Iri, Json, []).

json_download(Iri, Json, Opts1):-
  merge_options([request_header('Accept','application/json')], Opts1, Opts2),
  http_get(Iri, json_read_dict0(Json, Opts1), Opts2).

json_read_dict0(Json, Opts, _, Read):-
  json_read_dict(Read, Json, Opts).



%! xml_download(+Iri, -Dom) is det.
%! xml_download(+Iri, -Dom, +Opts) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given IRI.

xml_download(Iri, Dom):-
  xml_download(Iri, Dom, []).

xml_download(Iri, Dom, Opts):-
  http_get(Iri, load_xml0(Dom, Opts)).

load_xml0(Dom, Opts, _, Read):-
  load_xml(Read, Dom, Opts).
