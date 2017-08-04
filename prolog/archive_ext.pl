:- module(
  archive_ext,
  [
    archive_path/2 % +UriSpec, -Path
  ]
).
:- reexport(library(archive)).

/** <module> Archive extensions

@author Wouter Beek
@version 2017/07
*/

:- use_module(library(stream_ext)).
:- use_module(library(uri/uri_ext)).





%! archive_path(+UriSpec:compound, -Path:list(dict)) is nondet.

archive_path(UriSpec, Metadata) :-
  call_on_uri(UriSpec, true_metadata(Metadata)).
