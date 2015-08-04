:- module(
  http_download,
  [
    file_download/3 % +Uri:atom
                    % ?File:atom
                    % +Options:list(compound)
    html_download/2, % +Uri:atom
                     % -Dom:list(compound)
    json_download/2, % +Uri:atom
                     % -Json:dict
    xml_download/2 % +Uri:atom
                   % -Dom:list(compound)
  ]
).

/** <module> HTTP download

Support for downloading files and datastructures over HTTP(S).

---

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(error)).
:- use_module(library(http/http_request)).
:- use_module(library(lambda)).
:- use_module(library(io/file_ext)).
:- use_module(library(option)).
:- use_module(library(sgml)).
:- use_module(library(uri/uri_file_name)).

:- predicate_options(file_download/3, 3, [
     freshness_lifetime(+or([between(0.0,inf),oneof([inf])])),
     pass_to(http_get/3, 3)
   ]).
:- predicate_options(load_html0/4, 2, [
     pass_to(load_html/3, 3)
   ]).





%! file_download(+Uri:atom, ?File:atom, +Options:list(compound)) is det.
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
% @see uri_nested_file/3 for how the file name is created based on the URI.
%
% @throws type_error if Uri is not an absolute URI.

% The file was already downloaded in the past.
file_download(Uri, File, Opts):-
  nonvar(File),
  exists_file(File), !,
  (   option(freshness_lifetime(FL), Opts, inf),
      is_fresh_file(File, FL)
  ->  access_file(File, read)
  ;   delete_file(File),
      file_download(Uri, File, Opts)
  ).
% Throw an exception if Uri is not an absolute URI.
file_download(Uri, File, Opts):-
  \+ uri_is_global(Uri), !,
  type_error(absolute_uri, Uri).
% A file name is given.
file_download(Uri, File0, Opts):-
  nonvar(File0), !,
  absolute_file_name(File0, File, [access(write)]),
  file_directory_name(File, Dir),
  make_directory_path(Dir),

  % Multiple threads could be downloading the same file,
  % so we cannot download to the file's systematic name.
  % Instead we save to a thread-specific name.
  thread_self(Id),
  atomic_list_concat([tmp,Id], '_', ThreadName),
  file_name_extension(File, ThreadName, TmpFile),

  % The actual downloading part.
  http_get(Uri, \Status^Read^write_stream_to_file(Read, TmpFile), Opts),

  % Give the file its original name.
  rename_file(TmpFile, File).
% No file name is given.
% Create a file name based on the given URI.
file_download(Uri, File, Opts):-
  uri_nested_file(Uri, File),
  file_download(Uri, File, Opts).



%! html_download(+Uri:atom, -Dom:list(compound)) is det.

html_download(Uri, Dom):-
  Opts = [dialect(html5),max_errors(-1)],
  http_get(Uri, \Status^Read^load_html(Read, Dom, Opts)).



%! json_download(+Uri:atom, -Json:dict) is det.

json_download(Uri, Json):-
  http_get(Uri, \Status^Read^json_read_dict(Read, Json)).



%! xml_download(+Uri:atom, -Dom:list(compound)) is det.
% Returns the HTML Document Object Model (DOM)
% for the website with the given URI.

xml_download(Uri, Dom):-
  http_get(Uri, \Status^Read^load_xml(Read, Dom, [])).
