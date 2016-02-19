:- module(
  archive_ext,
  [
    archive_nth0_entry/4 % +Index:nonneg
                         % +Archive:blob
                         % -EntryName:atom
                         % -Read:blob
  ]
).

/** <module> Archive extraction

Extensions to SWI-Prolog's library archive.

@author Wouter Beek
@tbd Remove dependency of plTree.
@version 2014/04, 2014/06-2014/08, 2014/10, 2015/02, 2015/09
*/

:- use_module(library(archive)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(hash_ext)).
:- use_module(library(http/http_open)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(rdf/rdf_api)).
:- use_module(library(zlib)).

:- thread_local
   entry_property/2.





%! archive_nth0_entry(
%!   +Index:nonneg,
%!   +Archive:blob,
%!   -EntryName:atom,
%!   -Read:blob
%! ) is det.

archive_nth0_entry(0, Archive, EntryName, Read):- !,
  archive_next_header(Archive, EntryName),
  archive_open_entry(Archive, Read).
archive_nth0_entry(Index1, Archive, EntryName, Read):-
  archive_next_header(Archive, _),
  succ(Index2, Index1),
  archive_nth0_entry(Index2, Archive, EntryName, Read).





% HELPERS %

archive_filters(Archive, Filters):-
  archive_property(Archive, filters(Filters)), !.
archive_filters(_, []).



%! source_directory_name(+Source, -Directory) is det.
% Returns an appropriate directory name for the given source.
%
% The source can be either an absolute file name or a URI.

source_directory_name(File, Dir):-
  is_absolute_file_name(File), !,
  file_directory_name(File, Dir).
source_directory_name(Iri, Dir):-
  is_iri(Iri), !,
  md5(Iri, Md5),
  absolute_file_name(Md5, Dir),
  make_directory_path(Dir).
