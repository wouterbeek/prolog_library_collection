:- module(
  file_ext,
  [
    append_directories/2,         % +Directories, -Directory
    append_directories/3,         % +Directory1, +Directory2, -Directory3
    call_to_file/2,               % +File, :Goal_3
    call_to_file/3,               % +File, :Goal_3, +Options
    compress_file/1,              % +File
    compress_file/2,              % +File, ?CompressedFile
    concatenate_files/2,          % +Files, +ConcatenatedFile
    create_directory/1,           % +Directory
    create_file_directory/1,      % +File
    delete_files_by_extension/1,  % +Extension
    directory_file/2,             % +Directory, -File
    directory_file_path2/3,       % ?Directory, ?File, ?Path
    directory_path/2,             % ?Directory, ?Path
    directory_path_recursive/2,   % +Directory, -Path
    directory_subdirectories/2,   % ?Directory, ?Subdirectories
    file_extensions/2,            % +File, -Extensions
    file_extensions_media_type/2, % +Extensions, -MediaType
    file_mode/2,                  % +File, +Mode
    file_name_extensions/3,       % ?File, ?Name, ?Extensions
    file_to_string/2,             % +File, -String
    is_empty_directory/1,         % +Directory
    media_type_extension/2,       % +MediaType, -Extension
    sort_file/1,                  % +File
    touch/1,                      % +File
    uchardet_file/2,              % +File, -Enc
    working_directory/1           % -Dir
  ]
).
:- reexport(library(filesex)).
:- reexport(library(stream_ext)).

/** <module> File extensions

@author Wouter Beek
@version 2017/04-2017/10
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dict_ext)).
:- use_module(library(default)).
:- use_module(library(error)).
:- use_module(library(http/rfc7231)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(stream_ext)).
:- use_module(library(string_ext)).
:- use_module(library(zlib)).

:- meta_predicate
    call_to_file(+, 3),
    call_to_file(+, 3, +),
    call_to_file(+, 3, -, +).

:- multifile
    error:has_type/2,
    file_ext:media_type_extension_/2.

error:has_type(directory, Directory) :-
  var(Directory), !,
  instantiation_error(Directory).
error:has_type(directory, Directory) :-
  \+ exists_directory(Directory), !,
  existence_error(directory, Directory).
error:has_type(directory, Directory) :-
  error:has_type(atom, Directory).
error:has_type(media_type, media(Supertype/Subtype,Parameters)) :-
  maplist(error:has_type(atom), [Supertype,Subtype]),
  error:has_type(list(compound), Parameters).

file_ext:media_type_extension_(media(application/'xhtml+xml',[]), xhtml).
file_ext:media_type_extension_(media(text/html,[]), html).





%! append_directories(+Directories:list(atom_, -Directory:atom) is det.

append_directories([], '/') :- !.
append_directories([H], H) :- !.
append_directories([H|T], Dir) :-
  append_directories_(H, T, Dir).

append_directories_(Dir, [], Dir) :- !.
append_directories_(Dir1, [H|T], Dir3) :-
  append_directories(Dir1, H, Dir2),
  append_directories_(Dir2, T, Dir3).


%! append_directories(+Directory1:atom, +Directory2:atom,
%!                    -Directory:atom) is det.
%
% Returns the directory name obtained by concatenating the given
% directory names.
%
% The empty atom in the first position indicates the root directory.
%
% Does *not* ensure that any of the directories exist.

append_directories(Dir1, Dir2, Dir3) :-
  directory_subdirectories(Dir1, Subdirs1),
  directory_subdirectories(Dir2, Subdirs2),
  append(Subdirs1, Subdirs2, Subdirs3),
  directory_subdirectories(Dir3, Subdirs3).



%! call_to_file(+File, :Goal_3) is det.
%! call_to_file(+File, :Goal_3, +Options:list(compound)) is det.
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

call_to_file(File, Goal_3) :-
  call_to_file(File, Goal_3, []).


call_to_file(File, Goal_3, Options) :-
  call_to_file(File, Goal_3, Metadata, Options),
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




% directory_file(+Directory:atom, -File:atom) is nondet.
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



%! directory_path_recursive(+Directory:atom, -Path:atom) is nondet.

directory_path_recursive(Dir, Path) :-
  directory_path(Dir, Path0),
  (   exists_directory(Path0)
  ->  directory_path_recursive(Path0, Path)
  ;   Path = Path0
  ).



%! directory_subdirectories(+Directory:atom, -Subdirectories:list(atom)) is det.
%! directory_subdirectories(-Directory:atom, +Subdirectories:list(atom)) is det.
%
% Occurrences of `.` and `..` in `Directory' are resolved.
%
% The empty atom in the first position indicates the root directory.
%
% For absolute directory names the first subdirectory name is the
% empty atom.

directory_subdirectories(Dir, Subdirs2) :-
  ground(Dir), !,
  atomic_list_concat(Subdirs1, /, Dir),
  resolve_subdirectories(Subdirs1, Subdirs2).
directory_subdirectories(Dir, Subdirs1) :-
  resolve_subdirectories(Subdirs1, Subdirs2),
  atomic_list_concat(Subdirs2, /, Dir).



%! file_extensions(+File:atom, -Extensions:list(atom)) is det.

file_extensions(File, Extensions) :-
  file_name_extensions(File, _, Extensions).



%! file_extensions_media_type(+Extensions:list(atom),
%!                            -MediaType:media_type) is det.

file_extensions_media_type(Extensions, MediaType) :-
  member(Extension1, Extensions),
  media_type_extension(MediaType, Extension1), !.



%! file_mode(+File:atom, +Mode:oneof([append,read,write])) is det.
%
% @throws existence_error
% @throws permission_error

file_mode(File, Mode) :-
  (   exists_file(File)
  ->  (   access_file(File, Mode)
      ->  true
      ;   permission_errro(Mode, file, File)
      )
  ;   existence_error(file, File)
  ).



%! file_name_extensions(+File:atom, -Name:atom, -Extensions:list(atom)) is det.
%! file_name_extensions(-File:atom, +Name:atom, +Extensions:list(atom)) is det.

file_name_extensions(File, Name, Extensions) :-
  atomic_list_concat([Name|Extensions], ., File).



%! file_to_string(+File:atom, -String:string) is det.

file_to_string(File, String) :-
  read_file_to_string(File, Codes, []),
  string_codes(String, Codes).



%! is_dummy_file(+File:atom) is semidet.

is_dummy_file(.).
is_dummy_file(..).



%! is_empty_directory(+Directory:atom) is semidet.

is_empty_directory(Dir) :-
  exists_directory(Dir),
  \+ directory_file(Dir, _).



%! media_type_extension(+MediaType:compound, -Extension:atom) is det.

media_type_extension(MediaType, Extension) :-
  once(media_type_extension_(MediaType, Extension)).

media_type_extension_(media(application/json,_), json).
media_type_extension_(media(application/'n-quads',_), nq).
media_type_extension_(media(application/'n-triples',_), nt).
media_type_extension_(media(application/'sparql-query',_), rq).
media_type_extension_(media(application/'x-prolog',_), pl).
media_type_extension_(media(image/jpeg,_), jpeg).
media_type_extension_(media(image/png,_), png).
media_type_extension_(media(text/turtle,_), ttl).



%! resolve_subdirectories(+Subdirectories1:list(atom),
%!                        -Subdirectories2:list(atom)) is det.
%
% Resolves `.' and `..'.

resolve_subdirectories([], []) :- !.
resolve_subdirectories([''], []) :- !.
resolve_subdirectories([.|T1], T2) :- !,
  resolve_subdirectories(T1, T2).
resolve_subdirectories([_,..|T1], T2) :- !,
  resolve_subdirectories(T1, T2).
resolve_subdirectories([H|T1], [H|T2]) :-
  resolve_subdirectories(T1, T2).



%! sort_file(+File) is det.

sort_file(FileSpec) :-
  absolute_file_name(FileSpec, File, [access(read)]),
  run_process(sort, ['-u','-o',file(File),file(File)], [env(['LC_ALL'='C'])]).



%! touch(+File) is det.

touch(File) :-
  call_to_file(File, true_metadata).



%! uchardet_file(+File, -Encoding:atom) is det.

uchardet_file(FileSpec, Enc2) :-
  absolute_file_name(FileSpec, File, [access(write)]),
  setup_call_cleanup(
    (
      process_create(
        path(uchardet),
        [file(File)],
        [stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
      ),
      thread_create(print_err(ProcErr), _, [detached(true)])
    ),
    (
      read_string(ProcOut, Encs),
      split_string(Encs, "", "\n", [Enc1|_])
    ),
    close(ProcOut)
  ),
  normalize_encoding(Enc1, Enc2).



%! working_directory(-Directory:atom) is det.

working_directory(Directory) :-
  working_directory(Directory, Directory).
