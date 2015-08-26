:- module(
  http_download,
  [
    file_download/2, % +Iri:iri
                     % ?File:atom
    file_download/3, % +Iri:iri
                     % ?File:atom
                     % +Options:list(compound)
    html_download/2, % +Iri:iri
                     % -Dom:list(compound)
    json_download/2, % +Iri:iri
                     % -Json:dict
    xml_download/2 % +Iri:iri
                   % -Dom:list(compound)
  ]
).

/** <module> HTTP download

Support for downloading files and datastructures over HTTP(S).

---

@author Wouter Beek
@tbd We cannot use library(lambda) because this copies the goal term,
     not returning the DOM argument.
@version 2015/07-2015/08
*/

:- use_module(library(error)).
:- use_module(library(http/http_request)).
:- use_module(library(http/json)).
:- use_module(library(os/file_ext)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(uri/uri_file_name)).

:- predicate_options(file_download/3, 3, [
     freshness_lifetime(+or([between(0.0,inf),oneof([inf])])),
     pass_to(http_get/3, 3)
   ]).





%! file_download(+Iri:atom, ?File:atom) is det.

file_download(Iri, File):-
  file_download(Iri, File, []).

%! file_download(+Iri:atom, ?File:atom, +Options:list(compound)) is det.
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
file_download(Iri, File0, Opts):-
  nonvar(File0), !,
  absolute_file_name(File0, File, [access(write)]),
  file_directory_name(File, Dir),
  make_directory_path(Dir),

  % Multiple threads could be downloading the same file,
  % so we cannot download to the file's systematic name.
  % Instead we save to a thread-specific name.
  thread_file(File, TmpFile),

  % The actual downloading part.
  http_get(Iri, write_stream_to_file0(TmpFile), Opts),

  % Give the file its original name.
  rename_file(TmpFile, File).
% No file name is given.
% Create a file name based on the given IRI.
file_download(Iri, File, Opts):-
  nested_uri_file_name(Iri, File),
  file_download(Iri, File, Opts).
write_stream_to_file0(File, _, _, Read):- write_stream_to_file(Read, File).



%! html_download(+Iri:atom, -Dom:list(compound)) is det.

html_download(Iri, Dom):-
  Opts = [dialect(html5),max_errors(-1),space(default)],
  http_get(Iri, load_html0(Dom, Opts)).
load_html0(Dom, Opts, _, _, Read):- load_html(Read, Dom, Opts).



%! json_download(+Iri:atom, -Json:dict) is det.

json_download(Iri, Json):-
  http_get(Iri, json_read_dict0(Json)).
json_read_dict0(Json, _, _, Read):- json_read_dict(Read, Json).



%! xml_download(+Iri:atom, -Dom:list(compound)) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given IRI.

xml_download(Iri, Dom):-
  http_get(Iri, load_xml0(Dom)).
load_xml0(Dom, _, _, Read):- load_xml(Read, Dom, []).
