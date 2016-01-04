:- module(
  archive_ext,
  [
    archive_create/2, % +File:atom
                      % -CompressedFile:atom
    archive_extract/4, % +Source
                       % ?Directory:atom
                       % -ArchiveFilters:list(atom)
                       % -EntryProperties:list(pair(atom,list(nvpair)))
    archive_goal/2, % +Source
                    % :Goal
    archive_goal/3, % +Source
                    % :Goal
                    % +Arguments:list
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
:- use_module(library(semweb/rdf_db)).
:- use_module(library(zlib)).

:- thread_local(entry_property/2).

:- meta_predicate(archive_goal(+,1)).
:- meta_predicate(archive_goal(+,2,+)).
:- meta_predicate(archive_goal(+,3,+,+)).
:- meta_predicate(archive_goal0(+,1)).
:- meta_predicate(archive_goal0(+,2,+)).
:- meta_predicate(archive_goal0(+,3,+,+)).





%! archive_create(+File:atom, -CompressedFile:atom) is det.
% Compress the given file using gzip.

archive_create(File, CompressedFile):-
  % The compressed file can be either set or not.
  (   var(CompressedFile)
  ->  file_kind_alternative(File, gzip, CompressedFile)
  ;   true
  ),

  setup_call_cleanup(
    gzopen(CompressedFile, write, Write, [format(gzip)]),
    setup_call_cleanup(
      open(File, read, Read),
      copy_stream_data(Read, Write),
      close(Read)
    ),
    close(Write)
  ).


%! archive_extract(
%!   +Source,
%!   ?Directory:atom,
%!   -ArchiveFilters:list(atom),
%!   -EntryProperties:list(pair(atom,list(nvpair)))
%! ) is det.
% Extracts the given file into the given directory.
%
% In case no directory is given, the directory of the given source is used.
%
% @throws type_error When `Source` is neither an absolute file name nor a URL.
% @throws instantiation_error When File is a variable.

archive_extract(Source, Dir, Filters, EntryPairs2):-
  default_goal(source_directory_name(Source), Dir),
  archive_goal(Source, archive_extract0, Filters, Dir),
  findall(
    EntryName-EntryProperty,
    retract(entry_property(EntryName, EntryProperty)),
    EntryPairs1
  ),
  group_pairs_by_key(EntryPairs1, EntryPairs2).

archive_extract0(Archive, Filters, Dir):-
  archive_filters(Archive, Filters),
  repeat,
  (   archive_next_header(Archive, RelativeFile),
      forall(
        archive_header_property(Archive, Property),
        assert(entry_property(RelativeFile, Property))
      )
  ->  setup_call_cleanup(
        archive_open_entry(Archive, Read),
        (
          relative_file_name(File, Dir, RelativeFile),
          % Directory files are re-created.
          % Non-directory files are copied from stream.
          (   entry_property(RelativeFile, filetype(directory))
          ->  make_directory_path(File)
          ;   create_file_directory(File),
              write_stream_to_file(Read, File)
          ),
          debug(archive_ext, 'Extracted entry ~a', [File])
        ),
        close(Read)
      ),
      fail
  ;   !,
      true
  ).





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



%! source_directory_name(+Source:atom, -Directory:atom) is det.
% Returns the directory for the given source.
%
% The source can be either an absolute file name or a URL.

source_directory_name(File, Dir):-
  is_absolute_file_name(File), !,
  file_directory_name(File, Dir).
source_directory_name(Iri, Dir):-
  is_iri(Iri), !,
  md5(Iri, Md5),
  absolute_file_name(Md5, Dir),
  make_directory_path(Dir).
