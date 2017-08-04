:- module(
  file_ext,
  [
    call_to_file/2,               % +FileSpec, :Goal_3
    call_to_file/3,               % +FileSpec, :Goal_3, +Options
    compress_file/1,              % +File
    compress_file/2,              % +File, ?CompressedFile
    concatenate_files/2,          % +Files, +ConcatenatedFile
    create_directory/1,           % +Dir
    create_file_directory/1,      % +File
    delete_files_by_extension/1,  % +Extension
    directory_file/2,             % +Dir, -File
    directory_file_path2/3,       % ?Dir, ?File, ?Path
    directory_path/2,             % ?Dir, ?Path
    directory_path_recursive/2,   % +Dir, -Path
    download/2,                   % +Uri, +FileSpec
    download/3,                   % +Uri, +FileSpec, +Options
    file_extensions/2,            % +File, -Extensions
    file_extensions_media_type/2, % +Extensions, -MediaType
    file_name_extensions/3,       % ?File, ?Name, ?Extensions
    file_to_string/2,             % +File, -String
    stream_metadata/2,            % +Stream, -Dict
    stream_to_file/4,             % +FileSpec, +In, +Metadata1, -Metadata2
    working_directory/1           % -Dir
  ]
).
:- reexport(library(filesex)).
:- reexport(library(stream_ext)).

/** <module> File extensions

@author Wouter Beek
@version 2017/04-2017/07
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(http/rfc7231)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(stream_ext)).
:- use_module(library(zlib)).

:- dynamic
    error:has_type/2,
    file_ext:media_type_extension/2.

:- meta_predicate
    call_to_file(+, 3),
    call_to_file(+, 3, +),
    call_to_file(+, 3, -, +).

:- multifile
    error:has_type/2,
    file_ext:media_type_extension/2.

error:has_type(directory, Directory) :-
  var(Directory), !,
  instantiation_error(Directory).
error:has_type(directory, Directory) :-
  \+ exists_directory(Directory), !,
  existence_error(directory, Directory).
error:has_type(directory, Directory) :-
  error:has_type(atom, Directory).

file_ext:media_type_extension(media(application/'xhtml+xml',[]), xhtml).
file_ext:media_type_extension(media(text/html,[]), html).





%! call_to_file(+FileSpec:compound, :Goal_3) is det.
%! call_to_file(+FileSpec:compound, :Goal_3, +Options:list(compound)) is det.
%
% The following options are supported:
%
%   * mode(+oneof([append,write]))
%
%     Default is `write'.
%
%   * metadata(-list(dict))
%
%   * Other options are passed to open/4 and call_to_stream/4.

call_to_file(FileSpec, Goal_3) :-
  call_to_file(FileSpec, Goal_3, []).


call_to_file(FileSpec, Goal_3, Options) :-
  call_to_file(FileSpec, Goal_3, Metadata, Options),
  ignore(option(metadata(Metadata), Options)).


call_to_file(FileSpec, Goal_3, Metadata, Options) :-
  option(mode(Mode), Options, write),
  absolute_file_name(FileSpec, File, [access(Mode)]),
  setup_call_cleanup(
    open(File, Mode, Out, Options),
    stream_ext:call_to_stream(Out, Goal_3, Metadata, Options),
    close(Out)
  ).



%! compress_file(+File) is det.
%! compress_file(+File, ?CompressedFile) is det.

compress_file(File) :-
  compress_file(File, _).


compress_file(File, CompressedFile) :-
  (   var(CompressedFile)
  ->  file_name_extension(File, gz, CompressedFile)
  ;   true
  ),
  file_name_extension(CompressedFile, tmp, TmpFile),
  setup_call_cleanup(
    (
      open(File, read, In),
      gzopen(TmpFile, write, Out, [format(gzip)])
    ),
    copy_stream_data(In, Out),
    (
      close(Out),
      close(In)
    )
  ),
  rename_file(TmpFile, CompressedFile).



%! concatenate_files(+Files, +ConcatenatedFile) is det.

concatenate_files(Files, File) :-
  setup_call_cleanup(
    open(File, write, Out),
    concatenate_files0(Files, Out),
    close(Out)
  ).

concatenate_files0([], _) :- !.
concatenate_files0([H|T], Out) :- !,
  setup_call_cleanup(
    open(H, read, In),
    copy_stream_data(In, Out),
    close(In)
  ),
  concatenate_files0(T, Out).



%! create_directory(+Dir) is det.

create_directory(Dir) :-
  exists_directory(Dir), !.
create_directory(Dir) :-
  make_directory_path(Dir).



%! create_file_directory(+Path) is det.
%
% Ensures that the directory structure for the given file exists.

create_file_directory(Path) :-
  (exists_directory(Path) -> Dir = Path ; directory_file_path(Dir, _, Path)),
  create_directory(Dir).



%! delete_file_silent(+File) is det.
%
% Succeed silently if File does not exist and print a message when
% it does exist and is deleted.

delete_file_silent(File) :-
  exists_file(File), !,
  delete_file(File).
delete_file_silent(_).



%! delete_files_by_extension(+Extension:atom) is det.
%! delete_files_by_extension(+Directory:atom, +Extension:atom) is det.

delete_files_by_extension(Extension) :-
  working_directory(Directory),
  delete_files_by_extension(Directory, Extension).


delete_files_by_extension(Directory, Extension) :-
  format(atom(Wildcard1), "*.~a", [Extension]),
  directory_file_path(Directory, Wildcard1, Wildcard2),
  expand_file_name(Wildcard2, Files),
  maplist(delete_file, Files).




% directory_file(+Dir, -File) is nondet.
%
% Non-deterministic variant of directory_files/2 that skips dummy
% files.

directory_file(Dir, File) :-
  directory_files(Dir, Files),
  member(File, Files),
  \+ is_dummy_file(File).



%! directory_file_path2(+Dir, -File, -Path) is nondet.
%! directory_file_path2(+Dir, +File, -Path) is det.
%! directory_file_path2(-Dir, -File, +Path) is det.

directory_file_path2(Dir, File, Path) :-
  ground(Dir), var(File), var(Path), !,
  directory_file(Dir, File),
  directory_file_path(Dir, File, Path).
directory_file_path2(Dir, File, Path) :-
  directory_file_path(Dir, File, Path).



%! directory_path(+Dir, -Path) is nondet.
%! directory_path(-Dir, +Path) is det.

directory_path(Dir, Path) :-
  directory_file(Dir, File),
  directory_file_path(Dir, File, Path).



%! directory_path_recursive(+Dir, -Path) is nondet.

directory_path_recursive(Dir, Path) :-
  directory_path(Dir, Path0),
  (   exists_directory(Path0)
  ->  directory_path_recursive(Path0, Path)
  ;   Path = Path0
  ).



%! download(+UriSpec:term, +FileSpec:term) is det.
%! download(+UriSpec:term, +FileSpec:term, +Options:list(compound)) is det.

download(UriSpec, FileSpec) :-
  download(UriSpec, FileSpec, []).


download(UriSpec, FileSpec, Options1) :-
  merge_options([decompression(false)], Options1, Options2),
  call_on_uri(UriSpec, stream_to_file(FileSpec), Options2).



%! file_extensions(+File, -Extensions) is det.

file_extensions(File, Extensions) :-
  file_name_extensions(File, _, Extensions).



%! file_extensions_media_type(+Extensions, -MediaType) is det.

file_extensions_media_type(Extensions, MediaType) :-
  member(Extension1, Extensions),
  file_ext:media_type_extension(MediaType, Extension1), !.



%! file_name_extensions(+File, -Name, -Extensions) is det.
%! file_name_extensions(-File, +Name, +Extensions) is det.

file_name_extensions(File, Name, Extensions) :-
  atomic_list_concat([Name|Extensions], ., File).



%! file_to_string(+File, -String) is det.

file_to_string(File, String) :-
  read_file_to_string(File, Codes, []),
  string_codes(String, Codes).



%! is_dummy_file(+File) is semidet.

is_dummy_file(.).
is_dummy_file(..).



%! stream_metadata(+Stream, -Dict) is det.

stream_metadata(Stream, Dict) :-
  stream_property(Stream, position(Pos)),
  stream_position_data(byte_count, Pos, NumBytes),
  stream_position_data(char_count, Pos, NumChars),
  stream_position_data(line_count, Pos, NumLines),
  stream_property(Stream, newline(Newline)),
  Dict = _{
    byte_count: NumBytes,
    char_count: NumChars,
    line_count: NumLines,
    newline: Newline
  }.



%! stream_to_file(+FileSpec:term, +In:stream, +Metadata1:list(dict),
%!                -Metadata2:list(dict)) is det.
%
% The file name File is based on the given Name, but supplemented by a
% file extension that is based on the Media Type in the `Content-Type'
% HTTP header (if present).

stream_to_file(FileSpec, In, Metadata, Metadata) :-
  (   metadata_content_type(Metadata, MediaType),
      media_type_extension(MediaType, Extension)
  ->  Options = [extensions([Extension])]
  ;   Options = []
  ),
  absolute_file_name(FileSpec, File, [access(write)|Options]),
  call_to_file(File, copy_stream_data(In), [compression(none),type(binary)]).

media_type_extension(MediaType, Extension) :-
  once(media_type_extension0(MediaType, Extension)).

media_type_extension0(media(application/json,_), json).
media_type_extension0(media(application/'n-quads',_), nq).
media_type_extension0(media(application/'n-triples',_), nt).
media_type_extension0(media(application/'sparql-query',_), rq).
media_type_extension0(media(application/'x-prolog',_), pl).
media_type_extension0(media(image/jpeg,_), jpeg).
media_type_extension0(media(image/png,_), png).
media_type_extension0(media(text/turtle,_), ttl).



%! working_directory(-Directory) is det.

working_directory(Directory) :-
  working_directory(Directory, Directory).
