:- encoding(utf8).
:- module(
  file_ext,
  [
    append_directories/2,         % +Directories, -Directory
    cat/2,                        % +Out, +Files
    change_file_name_extension/4, % +File1, +Extension1, +Extension2, +File2
    compress_file/1,              % +FromFile
    compress_file/2,              % +FromFile, ?ToFile
    concatenate_files/2,          % +Files, +ConcatenatedFile
    convert_file/2,               % +File, +Format
    convert_file/3,               % +FromFile, +Format, ?ToFile
    create_directory/1,           % +Directory
    create_file_directory/1,      % +File
    delete_files_by_extension/1,  % +Extension
    delete_files_by_extension/2,  % +Directory, +Extension
    delete_files_by_extensions/1, % +Extensions
    delete_files_by_extensions/2, % +Directory, +Extensions
    directory_file/2,             % +Directory, -File
    directory_file_path2/3,       % ?Directory, ?File, ?Path
    directory_parent/2,           % +ChildDirectory, -ParentDirectory
    directory_path/2,             % ?Directory, ?Path
    directory_path_recursive/2,   % +Directory, -Path
    directory_subdirectories/2,   % ?Directory, ?Subdirectories
    directory_subdirectory/2,     % +Directory, ?Subdirectory
    directory_subdirectory/3,     % +Directory, ?Local, ?Subdirectory
    file_extension/2,             % +File, -Extension
    file_extensions/2,            % +File, -Extensions
    file_extensions_media_type/2, % +Extensions, -MediaType
    file_line/2,                  % +File, -Line
    file_media_type/2,            % +File, -MediaType
    file_mode/2,                  % +File, +Mode
    file_name/2,                  % ?File, ?Name
    file_name_extension2/3,       % ?File, ?Name, ?Extension
    file_name_extensions/3,       % ?File, ?Name, ?Extensions
    file_size/2,                  % +File, -Size
    file_to_string/2,             % +File, -String
    guess_file_encoding/2,        % +File, ?Encoding
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
    recode_file/1,                % +File
    recode_file/2,                % +File, +FromEncoding
    recode_files/2,               % +FromFile, +ToFile
    recode_files/3,               % +FromFile, +FromEncoding, +ToFile
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
@version 2017-2019
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
    call_files_(+, +, +, +, 2, +, +),
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



%! compress_file(+FromFile:atom) is det.
%! compress_file(+FromFile:atom, +ToFile:atom) is det.
%! compress_file(+FromFile:atom, -ToFile:atom) is det.

compress_file(FromFile) :-
  compress_file(FromFile, _).


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
%! convert_file(+FromFile:atom, +Format:atom, ?ToFile:atom) is det.
%
% @see Formats are ‘documented’ over at
% https://cgit.freedesktop.org/libreoffice/core/tree/filter/source/config/fragments/filters
%
% @see Encodings are ‘documented’ over at
% https://wiki.openoffice.org/wiki/Documentation/DevGuide/Spreadsheets/Filter_Options

convert_file(File, Format) :-
  convert_file(File, Format, _).


convert_file(FromFile, Format, ToFile) :-
  call_must_be(convert_format, Format),
  from_to_file_(FromFile, [Format], TmpFile),
  from_to_file_(FromFile, [Format], ToFile),
  file_directory_name(ToFile, ToDir),
  process_create(
    path(libreoffice),
    ['--convert-to',Format,'--infilter=CSV:44,34,76,1','--outdir',ToDir,file(FromFile)],
    []
  ),
  rename_file(TmpFile, ToFile).

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

delete_files_by_extension(Ext) :-
  working_directory(Dir),
  delete_files_by_extension(Dir, Ext).


delete_files_by_extension(Dir, Ext) :-
  format(atom(Wildcard1), "*.~a", [Ext]),
  directory_file_path(Dir, Wildcard1, Wildcard2),
  expand_file_name(Wildcard2, Files),
  maplist(delete_file, Files).



%! delete_files_by_extensions(+Extensions:list(atom)) is det.
%! delete_files_by_extensions(+Directory:atom, +Extensions:list(atom)) is det.

delete_files_by_extensions(Exts) :-
  working_directory(Dir),
  delete_files_by_extensions(Dir, Exts).


delete_files_by_extensions(Dir, Exts) :-
  threaded_maplist_1(delete_files_by_extension(Dir), Exts).



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



%! file_extension(+File:atom, -Extension:atom) is nondet.

file_extension(File, Ext) :-
  file_extensions(File, Exts),
  member(Ext, Exts).



%! file_extensions(+File:atom, -Extensions:list(atom)) is det.

file_extensions(File, Exts) :-
  file_name_extensions(File, _, Exts).



%! file_extensions_media_type(+Extensions:list(atom), -MediaType:media_type) is det.

file_extensions_media_type(Exts, MediaType) :-
  member(Ext, Exts),
  media_type_extension(MediaType, Ext), !.



%! file_line(+File:atom, -Line:string) is nondet.

file_line(File, Line) :-
  read_from_file(File, {Line}/[In]>>stream_line(In, Line)).



%! file_media_type(+File:atom, -MediaType:compound) is nondet.

file_media_type(File, MediaType) :-
  file_extension(File, Ext),
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



%! file_name(+File:atom, +Name:atom) is semidet.
%! file_name(+File:atom, -Name:atom) is det.

file_name(File, Name) :-
  file_name_extensions(File, Name, _).



%! file_name_extension2(+File:atom, -Name:atom, -Extension:atom) is det.
%! file_name_extension2(-File:atom, +Name:atom, +Extension:atom) is det.

file_name_extension2(File, Name, Ext) :-
  file_name_extensions(File, Name, [Ext]).



%! file_name_extensions(+File:atom, -Name:atom, -Extensions:list(atom)) is det.
%! file_name_extensions(-File:atom, +Name:atom, +Extensions:list(atom)) is det.

file_name_extensions(File, Name, Exts) :-
  atomic_list_concat([Name|Exts], ., File).



%! file_size(+File:atom, -Size:nonneg) is det.
%
% @see Wrapper around size_file/2.

file_size(File, Size) :-
  size_file(File, Size).



%! file_to_string(+File:atom, -String:string) is det.

file_to_string(File, String) :-
  read_file_to_string(File, Codes, []),
  string_codes(String, Codes).



%! guess_file_encoding(+File:atom, +Encoding:atom) is det.
%! guess_file_encoding(+File:atom, -Encoding:atom) is det.
%
% When Encoding is instantiated to an encoding different from the
% guessed encoding, the error unexpected_encoding/2 is thrown.
%
% @see guess_encoding/2.

guess_file_encoding(File, Enc1) :-
  read_from_file(
    File,
    {Enc2}/[In]>>guess_encoding(In, Enc2),
    [type(binary)]
  ),
  (   var(Enc1)
  ->  Enc1 = Enc2
  ;   stream_ext:clean_encoding_(Enc1, Enc3),
      Enc3 == Enc2
  ->  true
  ;   throw(error(unexpected_encoding(Enc2,Enc1),guess_file_encoding/2))
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



%! read_write_file(+File:atom, :Goal_2) is det.
%! read_write_file(+File:atom, :Goal_2, +ReadOptions:list(compound), +WriteOptions:list(compound)) is det.

read_write_file(File, Goal_2) :-
  read_write_file(File, Goal_2, []).


read_write_file(File, Goal_2, Options) :-
  read_write_file(File, Goal_2, Options, Options).


read_write_file(File, Goal_2, ReadOptions, WriteOptions) :-
  file_name_extensions(File, Base, FromExts),
  (   append(Exts, [gz], FromExts)
  ->  append(Exts, [tmp,gz], TmpExts),
      file_name_extensions(TmpFile, Base, TmpExts)
  ;   file_name_extension(File, tmp, TmpFile)
  ),
  read_write_files(File, TmpFile, Goal_2, ReadOptions, WriteOptions),
  rename_file(TmpFile, File).



%! read_write_files(+FromFile:atom, +ToFile:atom, :Goal_2) is det.
%! read_write_files(+FromFile:atom, +ToFile:atom, :Goal_2, +Options:list(compound)) is det.
%! read_write_files(+FromFile:atom, +ToFile:atom, :Goal_2, +ReadOptions:list(compound), +WriteOptions:list(compound)) is det.

read_write_files(FromFile, ToFile, Goal_2) :-
  read_write_files(FromFile, ToFile, Goal_2, []).


read_write_files(FromFile, ToFile, Goal_2, Options) :-
  read_write_files(FromFile, ToFile, Goal_2, Options, Options).


read_write_files(FromFile, ToFile, Goal_2, ReadOptions, WriteOptions) :-
  create_file_directory(ToFile),
  call_files_(FromFile, read, ToFile, write, Goal_2, ReadOptions, WriteOptions).



%! recode_file(+FromFile:atom) is det.
%! recode_file(+FromFile:atom, +FromEncoding:atom) is det.
%
% Recodes the given File from the given FromEncoding to UTF-8.

recode_file(File) :-
  guess_file_encoding(File, Enc),
  recode_file(File, Enc).


% Optimization: do not copy stream contents when the encoding remains
% the same.
recode_file(_, utf8) :- !.
recode_file(File, Enc) :-
  read_write_file(File, {Enc}/[In,Out]>>recode_stream(In, Enc, Out), [type(binary)]).



%! recode_files(+FromFile:atom, +ToFile:atom) is det.
%! recode_files(+FromFile:atom, +FromEncoding:atom, +ToFile:atom) is det.

recode_files(FromFile, ToFile) :-
  guess_file_encoding(FromFile, Enc),
  recode_files(FromFile, Enc, ToFile).


recode_files(FromFile, Enc1, ToFile) :-
  stream_ext:clean_encoding_(Enc1, Enc2),
  (   % Optimization: do not copy stream contents when the encoding
      % remains the same.
      Enc2 == utf8
  ->  true
  ;   read_write_files(FromFile, ToFile, {Enc2}/[In,Out]>>recode_stream(In, Enc2, Out), [type(binary)])
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

sort_file(File) :-
  sort_file(File, []).


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
%
% If File's directory does not exist it is created.

write_to_file(File, Goal_1) :-
  write_to_file(File, Goal_1, []).


write_to_file(File, Goal_1, Options) :-
  create_file_directory(File),
  call_file_(File, write, Goal_1, Options).





% GENERICS %

%! call_file_(+File:atom, +Mode:oneof([append,read,write]), :Goal_1, +Options:list(compound)) is det.

call_file_(File, Mode, Goal_1, Options) :-
  setup_call_cleanup(
    open_file_(File, Mode, Stream, Options),
    call(Goal_1, Stream),
    close(Stream)
  ).



%! call_files_(+File1:atom, +Mode1:oneof([append,read,write]), +File2:atom, +Mode2:oneof([append,read,write]), :Goal_2, +Options1:list(compound), +Options2:list(compound)) is det.

call_files_(File1, Mode1, File2, Mode2, Goal_2, Options1, Options2) :-
  setup_call_cleanup(
    maplist(
      open_file_,
      [File1,File2],
      [Mode1,Mode2],
      [Stream1,Stream2],
      [Options1,Options2]
    ),
    call(Goal_2, Stream1, Stream2),
    maplist(close, [Stream1,Stream2])
  ).



%! from_to_file_(+FromFile:atom, +Extensions:list(atom), +ToFile:atom) is semidet.
%! from_to_file_(+FromFile:atom, +Extensions:list(atom), -ToFile:atom) is det.

from_to_file_(FromFile, Exts, ToFile) :-
  var(ToFile), !,
  directory_file_path2(Dir, FromBase, FromFile),
  file_name(FromBase, Local),
  file_name_extensions(ToBase, Local, Exts),
  directory_file_path2(Dir, ToBase, ToFile).
from_to_file_(_, _, _).



%! open_file_(+File:atom, +Mode:oneof([append,read,write]), -Stream:stream, +Options:list(compoind)) is det.

open_file_(File, Mode, Stream2, Options) :-
  access_file(File, Mode),
  open_gz_(File, Mode, Stream1, Options),
  open_hash_(Stream1, Stream2, Options).



%! open_gz_(+File:atom, +Mode:oneof([append,read,write]), -Stream:stream, +Options:list(compound)) is det.

open_gz_(File, Mode, Stream, Options) :-
  file_name_extension(_, gz, File), !,
  gzopen(File, Mode, Stream, Options).
open_gz_(File, Mode, Stream, Options) :-
  open(File, Mode, Stream, Options).



%! open_hash_(+Stream1:stream, -Stream2:stream, +Options:list(compound)) is semidet.

open_hash_(Stream1, Stream2, Options) :-
  select_algorithm_option_(Algorithm, Options), !,
  open_hash_stream(Stream1, Stream2, [algorithm(Algorithm),close_parent(false)]).
open_hash_(Stream, Stream, _).

select_algorithm_option_(md5, Options) :-
  option(md5(_), Options).
select_algorithm_option_(sha1, Options) :-
  option(sha1(_), Options).
select_algorithm_option_(sha224, Options) :-
  option(sha224(_), Options).
select_algorithm_option_(sha256, Options) :-
  option(sha256(_), Options).
select_algorithm_option_(sha384, Options) :-
  option(sha384(_), Options).
select_algorithm_option_(sha512, Options) :-
  option(sha512(_), Options).
