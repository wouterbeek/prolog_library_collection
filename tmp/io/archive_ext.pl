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
    archive_info/1, % +Source
    archive_nth0_entry/4, % +Index:nonneg
                          % +Archive:blob
                          % -EntryName:atom
                          % -Read:blob
    archive_named_entry/3 % +EntryName:atom
                          % +Archive:blob
                          % -Read:blob
  ]
).

/** <module> Archive extraction

Extensions to SWI-Prolog's library archive.

@author Wouter Beek
@tbd Remove dependency of plTree.
@version 2014/04, 2014/06-2014/08, 2014/10, 2015/02
*/

:- use_module(library(archive)).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(http/http_open)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(pairs)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).
:- use_module(library(zlib)).

:- use_module(plc(generics/db_ext)).
:- use_module(plc(generics/meta_ext)).
:- use_module(plc(generics/typecheck)).
:- use_module(plc(io/dir_ext)).
:- use_module(plc(io/file_ext)).
:- use_module(plc(io/io_ext)).

:- thread_local(entry_property/2).

:- meta_predicate(archive_goal(+,1)).
:- meta_predicate(archive_goal(+,2,+)).
:- meta_predicate(archive_goal(+,3,+,+)).
:- meta_predicate(archive_goal0(+,1)).
:- meta_predicate(archive_goal0(+,2,+)).
:- meta_predicate(archive_goal0(+,3,+,+)).

:- db_add_novel(user:prolog_file_type(gz, gzip)).



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
          relative_file_path(File, Dir, RelativeFile),
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



%! archive_goal(+Source:atom, :Goal) is det.
%! archive_goal(+Source:atom, :Goal, ?Argument1) is det.
%! archive_goal(+Source:atom, :Goal, ?Argument1, ?Argument2) is det.
% `Source` is either an absolute file name or a URL.
%
% @throws type_error When `Source` is neither an absolute file name nor a URL.
% @throws instantiation_error When File is a variable.

archive_goal(Read, Goal):-
  is_stream(Read), !,
  archive_goal0(Read, Goal).
archive_goal(File, Goal):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, read, Read),
    archive_goal0(Read, Goal),
    close(Read)
  ).
archive_goal(Url, Goal):-
  is_uri(Url), !,
  setup_call_cleanup(
    http_open(Url, Read, []),
    archive_goal0(Read, Goal),
    close(Read)
  ).
archive_goal(Source, _):-
  type_error(file_or_url, Source).

archive_goal(Read, Goal, Arg1):-
  is_stream(Read), !,
  archive_goal0(Read, Goal, Arg1).
archive_goal(File, Goal, Arg1):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, read, Read),
    archive_goal0(Read, Goal, Arg1),
    close(Read)
  ).
archive_goal(Url, Goal, Arg1):-
  is_uri(Url), !,
  setup_call_cleanup(
    http_open(Url, Read, []),
    archive_goal0(Read, Goal, Arg1),
    close(Read)
  ).
archive_goal(Source, _, _):-
  type_error(file_or_url, Source).

archive_goal(Read, Goal, Arg1, Arg2):-
  is_stream(Read), !,
  archive_goal0(Read, Goal, Arg1, Arg2).
archive_goal(File, Goal, Arg1, Arg2):-
  is_absolute_file_name(File), !,
  setup_call_cleanup(
    open(File, read, Read),
    archive_goal0(Read, Goal, Arg1, Arg2),
    close(Read)
  ).
archive_goal(Url, Goal, Arg1, Arg2):-
  is_uri(Url), !,
  setup_call_cleanup(
    http_open(Url, Read, []),
    archive_goal0(Read, Goal, Arg1, Arg2),
    close(Read)
  ).
archive_goal(Source, _, _, _):-
  type_error(file_or_url, Source).


archive_goal0(Source, Goal):-
  setup_call_cleanup(
    archive_open(
      Source,
      Archive,
      [close_parent(false),filter(all),format(all),format(raw)]
    ),
    call(Goal, Archive),
    archive_close(Archive)
  ).

archive_goal0(Source, Goal, Arg1):-
  setup_call_cleanup(
    archive_open(
      Source,
      Archive,
      [close_parent(false),filter(all),format(all),format(raw)]
    ),
    call(Goal, Archive, Arg1),
    archive_close(Archive)
  ).

archive_goal0(Source, Goal, Arg1, Arg2):-
  setup_call_cleanup(
    archive_open(
      Source,
      Archive,
      [close_parent(false),filter(all),format(all),format(raw)]
    ),
    call(Goal, Archive, Arg1, Arg2),
    archive_close(Archive)
  ).


%! archive_info(+Source) is det.
% Writes archive information for the given file or URL to current input.
%
% ### Example
%
% ```prolog
% ?- absolute_file_name(data('abcde.tar.gz'), File, [access(read)]),
%    archive_info(File).
% ab.tar.gz
%   filetype(file)
%   mtime(1402126051.0)
%   size(128)
%   format(posix ustar format)
%   a.txt
%     filetype(file)
%     mtime(1402126033.0)
%     size(2)
%     format(posix ustar format)
%   b.txt
%     filetype(file)
%     mtime(1402126038.0)
%     size(2)
%     format(posix ustar format)
% cd.tar.gz
%   filetype(file)
%   mtime(1402126098.0)
%   size(128)
%   format(posix ustar format)
%   d.txt
%     filetype(file)
%     mtime(1402126074.0)
%     size(2)
%     format(posix ustar format)
%   c.txt
%     filetype(file)
%     mtime(1402126067.0)
%     size(2)
%     format(posix ustar format)
% e.txt
%   filetype(file)
%   mtime(1402126131.0)
%   size(2)
%   format(posix ustar format)
% File = '.../data/abcde.tar.gz'.
% ```

archive_info(Source):-
  archive_goal(Source, archive_info0, 0).

archive_info0(Archive, Indent1):-
  repeat,
  (
    archive_next_header(Archive, EntryName),
    \+ is_leaf_entry(Archive, EntryName)
  ->
    print_message(information, archive_entry(Indent1,Archive,EntryName)),
    succ(Indent1, Indent2),
    % Recurse archive entries.
    setup_call_cleanup(
      archive_open_entry(Archive, Stream),
      archive_goal0(Stream, archive_info0, Indent2),
      close(Stream)
    ),
    fail
  ; !,
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


%! archive_named_entry(+EntryName:atom, +Archive:blob, -Read:blob) is det.

archive_named_entry(EntryName, Archive, Read):-
  archive_next_header(Archive, EntryName0),
  (
    EntryName0 == EntryName
  ->
    archive_open_entry(Archive, Read)
  ;
    archive_named_entry(EntryName, Archive, Read)
  ).



% HELPERS %

archive_filters(Archive, Filters):-
  archive_property(Archive, filters(Filters)), !.
archive_filters(_, []).


is_leaf_entry(Archive, EntryName):-
  archive_header_property(Archive, format(EntryFormat)),
  EntryName == data,
  EntryFormat == raw.


%! is_uri(@Term) is semidet.

is_uri(Uri):-
  text(Uri),
  uri_components(Uri, UriComponents),
  uri_data(scheme, UriComponents, Scheme),
  nonvar(Scheme),
  memberchk(Scheme, [ftp,http,https]).



%! source_directory_name(+Source:atom, -Directory:atom) is det.
% Returns the directory for the given source.
%
% The source can be either an absolute file name or a URL.

source_directory_name(File, Dir):-
  is_absolute_file_name(File), !,
  file_directory_name(File, Dir).
source_directory_name(Url, Dir):-
  is_uri(Url), !,
  rdf_atom_md5(Url, 1, Md5),
  create_directory(data, [Md5], Dir).



% MESSAGES %

:- multifile(prolog:message//1).

prolog:message(archive_entry(Indent1,Archive,EntryName)) -->
  archive_header(Indent1, EntryName),
  {
    findall(
      Property,
      archive_header_property(Archive, Property),
      Properties
    ),
    succ(Indent1, Indent2)
  },
  archive_properties(Indent2, Properties).

archive_header(Indent, EntryName) -->
  indent(Indent),
  [EntryName,nl].

archive_properties(_, []) --> !, [].
archive_properties(Indent, [H|T]) -->
  indent(Indent),
  ['~w'-[H],nl],
  archive_properties(Indent, T).

indent(0) --> !, [].
indent(I1) -->
  ['  '],
  {succ(I2, I1)},
  indent(I2).

