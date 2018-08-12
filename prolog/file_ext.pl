:- encoding(utf8).
:- module(
  file_ext,
  [
    append_directories/2,         % +Directories, -Directory
    append_directories/3,         % +Directory1, +Directory2, -Directory3
    cat/2,                        % +Out, +Files
    change_file_name_extension/4, % +File1, +Extension1, +Extension2, +File2
    compress_file/2,              % +FromFile, ?ToFile
    concatenate_files/2,          % +Files, +ConcatenatedFile
    convert_file/2,               % +File, +Format
    convert_file/3,               % +File1, +Format, ?File2
    create_directory/1,           % +Directory
    create_file_directory/1,      % +File
    delete_files_by_extension/1,  % +Extension
    directory_file/2,             % +Directory, -File
    directory_file_path2/3,       % ?Directory, ?File, ?Path
    directory_parent/2,           % +ChildDirectory, -ParentDirectory
    directory_path/2,             % ?Directory, ?Path
    directory_path_recursive/2,   % +Directory, -Path
    directory_subdirectories/2,   % ?Directory, ?Subdirectories
    directory_subdirectory/2,     % +Directory, ?Subdirectory
    directory_subdirectory/3,     % +Directory, ?Local, ?Subdirectory
    file_extensions/2,            % +File, -Extensions
    file_extensions_media_type/2, % +Extensions, -MediaType
    file_line/2,                  % +File, -Line
    file_media_type/2,            % +File, -MediaType
    file_mode/2,                  % +File, +Mode
    file_name_extensions/3,       % ?File, ?Name, ?Extensions
    file_to_string/2,             % +File, -String
    guess_file_encoding/2,        % +File, -Encoding
    home_directory/1,             % ?Directory
    is_dummy_file/1,              % +File
    is_empty_directory/1,         % +Directory
    is_empty_file/1,              % +File
    peek_file/3,                  % +File, +Size, -String
    read_from_file/2,             % +File, :Goal_1
    read_from_file/3,             % +File, :Goal_1, +Options
    read_write_file/2,            % +FromFile, :Goal_2
    read_write_file/3,            % +FromFile, :Goal_2, +Options
    read_write_file/4,            % +FromFile, :Goal_2, +ReadOptions, +WriteOptions
    read_write_files/3,           % +FromFile, +ToFile, :Goal_2
    read_write_files/4,           % +FromFile, +ToFile, :Goal_2, +Options
    read_write_files/5,           % +FromFile, +ToFile, :Goal_2, +ReadOptions, +WriteOptions
    recode_file/2,                % +FromEncoding, +File
    sort_file/1,                  % +File
    sort_file/2,                  % +File, +Options
    touch/1,                      % +File
    working_directory/1,          % -Directory
    write_to_file/2,              % +File, :Goal_1
    write_to_file/3               % +File, :Goal_1, +Options
  ]
).
:- reexport(library(filesex)).
:- reexport(library(stream_ext)).

/** <module> File extensions

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(zlib)).

:- use_module(library(call_ext)).
:- use_module(library(media_type)).
:- use_module(library(sort_ext)).
:- use_module(library(stream_ext)).
:- use_module(library(thread_ext)).

:- meta_predicate
    call_file_(+, +, 1, +),
    read_from_file(+, 1),
    read_from_file(+, 1, +),
    read_write_file(+, 2),
    read_write_file(+, 2, +),
    read_write_file(+, 2, +, +),
    read_write_files(+, +, 2),
    read_write_files(+, +, 2, +),
    read_write_files(+, +, 2, +, +),
    write_to_file(+, 1),
    write_to_file(+, 1, +).

:- multifile
    error:has_type/2.

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





%! append_directories(+Directories:list(atom), -Directory:atom) is det.

append_directories([], '/') :- !.
append_directories([H], H) :- !.
append_directories([H|T], Dir) :-
  append_directories_(H, T, Dir).

append_directories_(Dir, [], Dir) :- !.
append_directories_(Dir1, [H|T], Dir3) :-
  append_directories(Dir1, H, Dir2),
  append_directories_(Dir2, T, Dir3).


%! append_directories(+Directory1:atom, +Directory2:atom, -Directory:atom) is det.
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



%! cat(+Out:stream, +Files:list(atom)) is det.

cat(Out, Files) :-
  maplist(cat_file(Out), Files).

cat_file(Out, File) :-
  read_from_file(File, {Out}/[In]>>copy_stream_data(In, Out)).



%! change_file_name_extension(+File1:atom, +Extension1:atom, +Extension2:atom, +File2:atom) is det.

change_file_name_extension(File1, Ext1, Ext2, File2) :-
  file_name_extension(Base, Ext1, File1),
  file_name_extension(Base, Ext2, File2).



%! compress_file(+FromFile:atom, +ToFile:atom) is det.
%! compress_file(+FromFile:atom, -ToFile:atom) is det.

compress_file(FromFile, ToFile) :-
  (var(ToFile) -> file_name_extension(FromFile, gz, ToFile) ; true),
  read_write_files(FromFile, ToFile, copy_stream_data).



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



%! convert_file(+File:atom, +Format:atom) is det.
%! convert_file(+File1:atom, +Format:atom, ?File2:atom) is det.
%
% @see Formats are ‘documented’ over at
% https://cgit.freedesktop.org/libreoffice/core/tree/filter/source/config/fragments/filters

convert_file(File, Format) :-
  convert_file(File, Format, _).


convert_file(File1, Format, File2) :-
  file_name_extension(Base, _, File1),
  file_name_extension(Base, Format, File2),
  call_must_be(convert_format, Format),
  process_create(path(libreoffice), ['--convert-to',Format,file(File1)], []).

convert_format(csv).



%! create_directory(+Directory:atom) is det.

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



%! directory_file_path2(+Directory:atom, +File:atom, -Path:atom) is det.
%! directory_file_path2(+Directory:atom, -File:atom, -Path:atom) is nondet.
%! directory_file_path2(-Directory:atom, -File:atom, +Path:atom) is det.
%
% Instantiation pattern (+,-,-) is not supported by
% directory_file_path/3 from the standard library.

directory_file_path2(Dir, File, Path) :-
  ground(Dir), var(File), var(Path), !,
  directory_file(Dir, File),
  directory_file_path(Dir, File, Path).
directory_file_path2(Dir, File, Path) :-
  directory_file_path(Dir, File, Path).



%! directory_parent(+ChildDirectory:atom, -ParentDirectory:atom) is det.

directory_parent(Dir1, Dir2) :-
  directory_subdirectories(Dir1, Subdirs1),
  once(append(Subdirs2, [_], Subdirs1)),
  directory_subdirectories(Dir2, Subdirs2).



%! directory_path(+Directory:atom, -Path:atom) is nondet.
%! directory_path(-Directory:atom, +Path:atom) is det.

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



%! directory_subdirectory(+Directory:atom, +SubDirectory:atom) is semidet.
%! directory_subdirectory(+Directory:atom, -SubDirectory:atom) is nondet.

directory_subdirectory(Dir, Subdir) :-
  directory_subdirectory(Dir, _, Subdir).


%! directory_subdirectory(+Directory:atom, +Local:atom, +SubDirectory:atom) is semidet.
%! directory_subdirectory(+Directory:atom, +Local:atom, -SubDirectory:atom) is semidet.
%! directory_subdirectory(+Directory:atom, -Local:atom, -SubDirectory:atom) is nondet.

directory_subdirectory(Dir, Local, Subdir) :-
  ground(Local), !,
  directory_file_path(Dir, Local, Subdir),
  exists_directory(Subdir).
directory_subdirectory(Dir, Local, Subdir) :-
  directory_path(Dir, Subdir),
  exists_directory(Subdir),
  directory_file_path(_, Local, Subdir).



%! file_extensions(+File:atom, -Extensions:list(atom)) is det.

file_extensions(File, Exts) :-
  file_name_extensions(File, _, Exts).



%! file_extensions_media_type(+Extensions:list(atom), -MediaType:media_type) is det.

file_extensions_media_type(Exts, MediaType) :-
  member(Extension1, Exts),
  media_type_extension(MediaType, Extension1), !.



%! file_line(+File:atom, -Line:string) is nondet.

file_line(File, Line) :-
  read_from_file(
    File,
    [In]>>(
      repeat,
      read_line_to_string(In, Line),
      (Line == end_of_file -> !, fail ; true)
    )
  ).



%! file_media_type(+File:atom, -MediaType:compound) is nondet.

file_media_type(File, MediaType) :-
  file_extensions(File, Exts),
  member(Ext, Exts),
  media_type_extension(MediaType, Ext).



%! file_mode(+File:atom, +Mode:oneof([append,read,write])) is det.
%
% @throws existence_error
% @throws permission_error

file_mode(File, Mode) :-
  (   exists_file(File)
  ->  (   access_file(File, Mode)
      ->  true
      ;   permission_error(Mode, file, File)
      )
  ;   existence_error(file, File)
  ).



%! file_name_extensions(+File:atom, -Name:atom, -Extensions:list(atom)) is det.
%! file_name_extensions(-File:atom, +Name:atom, +Extensions:list(atom)) is det.

file_name_extensions(File, Name, Exts) :-
  atomic_list_concat([Name|Exts], ., File).



%! file_to_string(+File:atom, -String:string) is det.

file_to_string(File, String) :-
  read_file_to_string(File, Codes, []),
  string_codes(String, Codes).



%! guess_file_encoding(+File:atom, -Encoding:atom) is det.

guess_file_encoding(File, Enc) :-
  read_from_file(
    File,
    {Enc}/[In]>>guess_encoding(In, Enc),
    [type(binary)]
  ).



%! home_directory(+Directory:atom) is semidet.
%! home_directory(-Directory:atom) is nondet.

home_directory(Dir) :-
  expand_file_name(~, [Dir]).



%! is_dummy_file(+File:atom) is semidet.

is_dummy_file(.).
is_dummy_file(..).



%! is_empty_directory(+Directory:atom) is semidet.

is_empty_directory(Dir) :-
  exists_directory(Dir),
  \+ directory_file(Dir, _).



%! is_empty_file(+File:atom) is semidet.

is_empty_file(File) :-
  read_from_file(File, at_end_of_stream).



%! peek_file(+File:atom, +Size:nonneg, -String:string) is det.

peek_file(File, Size, Str) :-
  read_from_file(File, {Size,Str}/[In]>>peek_string(In, Size, Str)).



%! read_from_file(+File:atom, :Goal_1) is det.
%! read_from_file(+File:atom, :Goal_1, +Options:list(compound)) is det.
%
% Calls Goal_1 on the input stream derived from the given File.  If
% the filen name ends in `.gz', GNU zip decompression is applied.

read_from_file(File, Goal_1) :-
  read_from_file(File, Goal_1, []).


read_from_file(File, Goal_1, Options) :-
  call_file_(File, read, Goal_1, Options).

call_file_(File, Mode, Goal_1, Options) :-
  setup_call_cleanup(
    open_(File, Mode, In, Options),
    call(Goal_1, In),
    close(In)
  ).

open_(File, Mode, Stream) :-
  open_(File, Mode, Stream, []).

open_(File, Mode, Stream, Options) :-
  file_name_extension(_, gz, File), !,
  gzopen(File, Mode, Stream, Options).
open_(File, Mode, Stream, Options) :-
  open(File, Mode, Stream, Options).



%! read_write_file(+FromFile:atom, :Goal_2) is det.
%! read_write_file(+FromFile:atom, :Goal_2, +ReadOptions:list(compound), +WriteOptions:list(compound)) is det.

read_write_file(FromFile, Goal_2) :-
  read_write_file(FromFile, Goal_2, []).


read_write_file(FromFile, Goal_2, Options) :-
  read_write_file(FromFile, Goal_2, Options, Options).


read_write_file(FromFile, Goal_2, ReadOptions, WriteOptions) :-
  file_name_extensions(FromFile, Base, FromExts),
  (   append(Exts, [gz], FromExts)
  ->  append(Exts, [tmp,gz], ToExts),
      file_name_extensions(ToFile, Base, ToExts)
  ;   ToFile = FromFile
  ),
  read_write_files(FromFile, ToFile, Goal_2, ReadOptions, WriteOptions).



%! read_write_files(+FromFile:atom, +ToFile:atom, :Goal_2) is det.
%! read_write_files(+FromFile:atom, +ToFile:atom, :Goal_2, +Options:list(compound)) is det.
%! read_write_files(+FromFile:atom, +ToFile:atom, :Goal_2, +ReadOptions:list(compound), +WriteOptions:list(compound)) is det.

read_write_files(FromFile, ToFile, Goal_2) :-
  read_write_files(FromFile, ToFile, Goal_2, []).


read_write_files(FromFile, ToFile, Goal_2, Options) :-
  read_write_files(FromFile, ToFile, Goal_2, Options, Options).


read_write_files(FromFile, ToFile, Goal_2, ReadOptions, WriteOptions) :-
  setup_call_cleanup(
    maplist(open_, [FromFile,ToFile], [read,write], [In,Out], [ReadOptions,WriteOptions]),
    call(Goal_2, In, Out),
    maplist(close, [In,Out])
  ),
  rename_file(ToFile, FromFile).



%! recode_file(+FromEncoding:atom, +File:atom) is det.
%
% Recodes the given File from the given FromEncoding to UTF-8.

% Noting to do: already in UTF-8.
recode_file(Enc, _) :-
  must_be(atom, Enc),
  memberchk(Enc, [ascii,utf8]), !.
% TBD: Is it _really_ impossible to reuse recode_stream/2 here?
recode_file(Enc, File) :-
  read_write_file(
    File,
    {Enc}/[In,Out]>>(
      process_create(
        path(iconv),
        ['-c','-f',Enc,'-t','utf-8',-],
        [stdin(pipe(ProcIn)),stdout(pipe(ProcOut))]
      ),
      maplist([Stream]>>set_stream(Stream, type(binary)), [ProcIn,ProcOut]),
      create_detached_thread(
        call_cleanup(
          copy_stream_data(In, ProcIn),
          close(ProcIn)
        )
      ),
      call_cleanup(
        copy_stream_data(ProcOut, Out),
        close(ProcOut)
      )
    ),
    [type(binary)]
  ).



%! resolve_subdirectories(+Subdirectories1:list(atom), -Subdirectories2:list(atom)) is det.
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



%! sort_file(+File:atom) is det.
%! sort_file(+File:atom, +Options:list(compound)) is det.

sort_file(File1) :-
  sort_file(File1, []).


sort_file(File, Options) :-
  read_write_file(
    File,
    {Options}/[In,Out]>>(
      sort_stream(In, ProcOut, Options),
      call_cleanup(
        copy_stream_data(ProcOut, Out),
        close(ProcOut)
      )
    )
  ).



%! touch(+File) is det.

touch(File) :-
  setup_call_cleanup(
    open(File, write, Out),
    true,
    close(Out)
  ).



%! working_directory(-Directory:atom) is det.

working_directory(Directory) :-
  working_directory(Directory, Directory).



%! write_to_file(+File:atom, :Goal_1) is det.
%! write_to_file(+File:atom, :Goal_1, +Options:list(compound)) is det.

write_to_file(File, Goal_1) :-
  write_to_file(File, Goal_1, []).


write_to_file(File, Goal_1, Options) :-
  call_file_(File, write, Goal_1, Options).
