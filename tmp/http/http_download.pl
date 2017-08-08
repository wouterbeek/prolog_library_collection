:- module(
  http_download,
  [
    file_download/2, % +Source, ?File
    file_download/3, % +Source, ?File, +Opts
    xml_download/2,  % +Source, -Dom
    xml_download/3   % +Source, -Dom,  +Opts
  ]
).

/** <module> HTTP download

Support for downloading files and datastructures over HTTP(S).

@author Wouter Beek
@version 2015/07-2015/11, 2016/01, 2016/04, 2016/12
*/

:- use_module(library(atom_ext)).
:- use_module(library(error)).
:- use_module(library(file_ext)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_ext)).
:- use_module(library(http/http_io)).
:- use_module(library(io)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(library(yall)).





%! file_download(+Uri, +File) is det.
%! file_download(+Uri, +File, +Opts) is det.
%
% Downloads the contents stored at the given URI to either the a File
% with either a given file name or a file name that is created based
% on the URI.
%
% The following options are supported:
%
%   * freshness_lifetime(+or([between(0.0,inf),oneof([inf])])) Sets
%   whether -- and if so, when -- files that were downloaded in the
%   past are redownloaded and overwritten.  Default is `inf`.
%
%   * Other options are passed to http_get/3.
%
% @throws type_error if Uri is not an absolute URI.

file_download(Uri, File) :-
  file_download(Uri, File, []).


% The file was already downloaded in the past.
file_download(Uri, File, Opts) :-
  nonvar(File),
  exists_file(File), !,
  (   option(freshness_lifetime(FL), Opts, inf),
      is_fresh_file(File, FL)
  ->  access_file(File, read)
  ;   delete_file(File),
      file_download(Uri, File, Opts)
  ).
% Throw an exception if Uri is not absolute.
file_download(Uri, _, _) :-
  \+ uri_is_global(Uri), !,
  type_error(absolute_uri, Uri).
% A file name is given.
file_download(Uri, File, Opts) :-
  iri_normalized(Uri, NormUri),
  md5(NormUri, Hash),
  thread_file(Hash, TmpFile),
  call_onto_stream(
    Uri,
    TmpFile,
    [In,InPath,InPath,Out]>>copy_stream_data(In, Out),
    [metadata(InPath)|Opts],
    Opts
  ),
  rename_file(TmpFile, File).



%! xml_download(+Source, -Dom) is det.
%! xml_download(+Source, -Dom, +Opts) is det.
%
% Returns the HTML Document Object Model (DOM) for the website with
% the given URI.

xml_download(Source, Dom) :-
  xml_download(Source, Dom, []).


xml_download(Source, Dom, Opts) :-
  http_get(Source, {Dom,Opts}/[In,InPath,InPath]>>load_xml(In, Dom, Opts)).
