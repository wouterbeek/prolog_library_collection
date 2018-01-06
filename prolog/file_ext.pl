:- encoding(utf8).
:- module(
  file_ext,
  [
    append_directories/2,         % +Directories, -Directory
    append_directories/3,         % +Directory1, +Directory2, -Directory3
    cat/2,                        % +Out, +Files
    compress_file/1,              % +File
    compress_file/2,              % +File, ?CompressedFile
    concatenate_files/2,          % +Files, +ConcatenatedFile
    convert_file/2,               % +File, +Format
    convert_file/3,               % +File1, +Format, ?File2
    create_directory/1,           % +Directory
    create_file_directory/1,      % +File
    delete_files_by_extension/1,  % +Extension
    directory_file/2,             % +Directory, -File
    directory_file_path2/3,       % ?Directory, ?File, ?Path
    directory_path/2,             % ?Directory, ?Path
    directory_path_recursive/2,   % +Directory, -Path
    directory_subdirectories/2,   % ?Directory, ?Subdirectories
    directory_subdirectory/2,     % +Dir, ?Subdir
    directory_subdirectory/3,     % +Dir, ?Local, ?Subdir
    file_extensions/2,            % +File, -Extensions
    file_extensions_media_type/2, % +Extensions, -MediaType
    file_mode/2,                  % +File, +Mode
    file_name_extensions/3,       % ?File, ?Name, ?Extensions
    file_to_string/2,             % +File, -String
    guess_file_encoding/2,        % +File, -Enc
    image_dimensions/2,           % +File, -Dimensions
    is_dummy_file/1,              % +File
    is_empty_directory/1,         % +Directory
    recode_file/1,                % +File
    recode_file/2,                % +File, +FromEncoding
    sort_file/1,                  % +File
    sort_file/2,                  % +File, +Options
    touch/1,                      % +File
    wc/4,                         % +File, -Lines, -Words, -Bytes
    working_directory/1           % -Dir
  ]
).
:- reexport(library(filesex)).
:- reexport(library(stream_ext)).

/** <module> File extensions

@author Wouter Beek
@version 2017/04-2018/01
*/

:- use_module(library(apply)).
:- use_module(library(call_ext)).
:- use_module(library(dcg/basics)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(media_type)).
:- use_module(library(option)).
:- use_module(library(os_ext)).
:- use_module(library(process)).
:- use_module(library(readutil)).
:- use_module(library(stream_ext)).
:- use_module(library(string_ext)).
:- use_module(library(thread_ext)).
:- use_module(library(zlib)).

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



%! cat(+Out:stream, +Files:list(atom)) is det.

cat(Out, Files) :-
  maplist(cat_file(Out), Files).

cat_file(Out, File) :-
  setup_call_cleanup(
    open(File, read, In),
    copy_stream_data(In, Out),
    close(In)
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
    maplist(close, [In,Out])
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
  create_process(libreoffice, ['--convert-to',Format,file(File1)]).

convert_format(csv).



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
  directory_file_path(_, Local, Subdir).



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



%! guess_file_encoding(+File:atom, -Enc:atom) is det.

guess_file_encoding(File, Enc) :-
  setup_call_cleanup(
    process_create(
      path(uchardet),
      [file(File)],
      [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
    ),
    (
      create_detached_thread(copy_stream_data(ProcErr, user_error)),
      read_string(ProcOut, String1),
      string_strip(String1, "\n", String2),
      normalize_encoding(String2, Enc),
      process_wait(Pid, exit(Status)),
      (Status =:= 0 -> true ; print_message(warning, process_status(Status)))
    ),
    maplist(close, [ProcErr,ProcOut])
  ).



%! image_dimensions(+File, -Dimensions:pair(float)) is det.
%
% @see Requires ImageMagick.

image_dimensions(File, Width-Height) :-
  process_create(
    path(identify),
    [file(File)],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  create_detached_thread(copy_stream_data(ProcErr, user_error)),
  read_stream_to_codes(ProcOut, Codes),
  phrase(image_dimensions_out(File, Width, Height), Codes, _),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))).

image_dimensions_out(File, Width, Height) -->
  atom(File),
  " ",
  ...,
  " ",
  integer(Width),
  "x",
  integer(Height),
  done.



%! is_dummy_file(+File:atom) is semidet.

is_dummy_file(.).
is_dummy_file(..).



%! is_empty_directory(+Directory:atom) is semidet.

is_empty_directory(Dir) :-
  exists_directory(Dir),
  \+ directory_file(Dir, _).



%! recode_file(+File:atom) is det.
%! recode_file(+File:atom, +FromEncoding:atom) is det.
%
% Recode to UTF-8 if File is a text file.

recode_file(File) :-
  guess_file_encoding(File, FromEnc),
  recode_file(File, FromEnc).


recode_file(File1, FromEnc) :-
  file_name_extension(File1, recoding, File2),
  create_process(iconv, ['-f',FromEnc,'-t','utf-8','-o',file(File2),file(File1)]),
  rename_file(File2, File1).



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



%! sort_file(+File:atom) is det.
%! sort_file(+File:atom, +Options:list(compound)) is det.

sort_file(File1) :-
  sort_file(File1, []).


sort_file(File1, Options) :-
  file_name_extension(File1, sorting, File2),
  setup_call_cleanup(
    maplist(open, [File1,File2], [read,write], [In,Out]),
    sort_stream(In, Out, Options),
    maplist(close, [In,Out])
  ),
  rename_file(File1, File2).



%! touch(+File) is det.

touch(File) :-
  setup_call_cleanup(
    open(File, write, Out),
    true,
    close(Out)
  ).



%! wc(+File:atom, -Lines:nonneg) is det.
%
% Native implementation of line count.

wc(File, Lines) :-
  setup_call_cleanup(
    open(File, read, In),
    (
      Counter = count(0),
      repeat,
      read_line_to_codes(In, Codes),
      (   Codes == end_of_file
      ->  !,
          arg(1, Counter, Lines)
      ;   arg(1, Counter, Count1),
          Count2 is Count1 + 1,
          nb_setarg(1, Counter, Count2),
          fail
      )
    ),
    close(In)
  ).



%! wc(+File:atom, -Lines:nonneg, -Words:nonneg, -Bytes:nonneg) is det.
%
% Linux-only parsing of GNU wc output.

wc(File, Lines, Words, Bytes) :-
  process_create(
    path(wc),
    [file(File)],
    [process(Pid),stderr(pipe(ProcErr)),stdout(pipe(ProcOut))]
  ),
  create_detached_thread(copy_stream_data(ProcErr, user_error)),
  read_stream_to_codes(ProcOut, Codes),
  phrase(wc_out(Lines, Words, Bytes), Codes, _),
  process_wait(Pid, exit(Status)),
  (Status =:= 0 -> true ; print_message(warning, process_status(Status))).

% E.g., `427  1818 13512 README.md`
wc_out(Lines, Words, Bytes) -->
  whites,
  integer(Lines),
  whites,
  integer(Words),
  whites,
  integer(Bytes).



%! working_directory(-Directory:atom) is det.

working_directory(Directory) :-
  working_directory(Directory, Directory).
