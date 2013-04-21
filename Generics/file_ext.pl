:- module(
  file_ext,
  [
    atom_to_file/2, % +Atom:atom
                    % +File:atom
    compiled_file/2, % +PL_File:atom
                     % -QLF_File:atom
    concat_path/3, % +Dir:atom
                   % +SubDir:atom
                   % -NewDir:atom
    create_directory/1, % +Directory:atom
    create_file/4, % +Directory:atom
                   % +Name:atom
                   % +Type:atom
                   % -File:atom
    file_name/4, % +File:atom
                 % ?Directory:atom
                 % ?Name:atom
                 % ?Extension:atom
    file_name_type/3, % ?Base:atom
                      % ?Type:atom
                      % ?Name:atom
    file_to_atom/2, % +File:file
                    % -Atom:atom
    file_type_alternative/3, % +FromFile:atom
                             % +ToFileType:atom
                             % -ToFile:atom
    nested_dir_name/2, % +RelativeDirs:compound
                       % -AbsoluteDir:atom
    path_walk_forest/3, % +Directories:list(atom)
                        % +RegularExpression:atom
                        % -AbsoluteFileNames:list(atom)
    path_walk_tree/3, % +RootDirectory:file
                      % +FileExtension:regex
                      % -Paths:list(file)
    read_terms/3, % +Stream:stream
                  % -Terms:list(term)
                  % +Options:list(atom)
    stream_to_atom/2, % +Stream:stream
                      % -Atom:atom
    stream_to_file/2 % +Stream:stream
                     % +File:atom
  ]
).

/** <module> File methods extending the standart SWI-Prolog repertoire.

Extra methods for creating, opening, removing, and searching files.

@author Wouter Beek
@version 2011/08-2012/05, 2012/09, 2013/04
*/

:- nodebug(file_ext).



%% atom_to_file(Atom, File) is det.
% Stores the given atom in the given file.
%
% @param Atom An atom.
% @param File A file.

atom_to_file(Atom, File):-
  open(File, write, Stream, [close_on_abort(true), type(text)]),
  format(Stream, '~w', [Atom]),
  close(Stream).

%% compiled_file(PL_File, QLF_File) is det.
% Returns the compiled version of the given Prolog file.
% Compiled files are called Quick Load Files (QLF).
% This method checks whether the Prolog file needs to be recompiled,
% or whether a previously compiled QLF file can be used.
%
% @param PL_File The atomic name of a Prolog file.
% @param QLF_File The atomic name of a QLF file.

compiled_file(PL_File, QLF_File):-
  file_name_type(Base, prolog,          PL_File),
  file_name_type(Base, quick_load_file, QLF_File),
  (
    exists_file(QLF_File),
    time_file(PL_File, PL_Time),
    time_file(QLF_File, QLF_Time),
    QLF_Time >= PL_Time
  ->
    true
  ;
    access_file(QLF_File, write)
  ->
    qcompile(PL_File)
  ;
    debug(file_ext, 'Cannot write to QLF, loading from ~w.', [PL_File])
  ).

%% concat_path(+Dir:atom, +SubDir:atom, -NewDir:atom) is det.
% Returns the give directory with the given subdirectory appended to it.
%
% @param Dir The atomic representation of a relative or absolute directory
%        path.
% @param SubDir The atomic name of a directory.
% @param NewDir The atomic representation of a relative or absolute directory
%        path.

concat_path(Dir, SubDir, NewDir):-
  atomic_list_concat([Dir, '/', SubDir], NewDir).

%% create_directory(+Directory:atom) is det.
% Creates a directory with the given name.
%
% @param Directory The atomic name of a directory.

create_directory(Directory):-
  exists_directory(Directory),
  !.
create_directory(Directory):-
  make_directory(Directory).

%% create_file(+Directory:term, +Name:atom, +Type:atom, -File:atom) is det.
% Creates a file with the given name, inside the given directory, and that
% is of the given file type.
% File types must be stored with prolog_file_type/2.
%
% @param Directory The atomic name of a directory or a compound term that
%        can be resolved by absolute_file_name/3, e.g. =|personal(audio)|=.
% @param Name The atomic *relative* name of a file without the extension,
%        i.e. a base name.
% @param Type The atomic name of a file type, as registered with
%        prolog_file_type/2, e.g. =|mp3|=.
% @param File The atomic *absolute* name of a file.

create_file(Directory, Name, Type, File):-
  % Resolve the directory in case the compound term notation employed
  % by absolute_file_name/3 is used.
  (
    compound(Directory)
  ->
    absolute_file_name(Directory, ResolvedDirectory)
  ;
    ResolvedDirectory = Directory
  ),

  % Make sure that the directory exists.
  create_directory(ResolvedDirectory),

  % Create the file name by appending the base and extension names.
  file_name_type(Name, Type, RelativeFile),

  % Append directory and file name.
  format(atom(File), '~w/~w', [ResolvedDirectory, RelativeFile]).

%% file_name(
%%   +File:atom,
%%   ?Directory:atom,
%%   ?Name:atom,
%%   ?Extension:atom
%% ) is semidet.
% The splitting of a file into its directory, local name and type parts.

file_name(File, Directory, Name, Extension):-
  file_directory_name(File, Directory),
  file_base_name(File, LocalFile),
  file_name_extension(Name, Extension, LocalFile).

%% file_name_type(?Name:atom, ?Type:atom, +File:atom) is semidet.
%% file_name_type(+Name:atom, +Type:atom, ?File:atom) is semidet.
% Decomposes a file name into its base name and its file type.
%
% @param Name The atomic name of a file, without a directory and without
%        an extension.
% @param Type An atomic file type. These are registered with
%        prolog_file_type/2.
% @param File The full name of a file.

file_name_type(Name, Type, File):-
  nonvar(Name),
  nonvar(Type),
  !,
  user:prolog_file_type(Extension, Type),
  file_name_extension(Name, Extension, File).
file_name_type(Name, Type, File):-
  nonvar(File),
  !,
  file_name_extension(Name, Extension, File),
  user:prolog_file_type(Extension, Type).

%% file_to_atom(+File:file, -Atom:atom) is det.
% Turns the given file's contents into a string.
%
% @param File The file whose contents are put in a string.
% @param Atom The atom containing the contents of the given file.

file_to_atom(File, Atom):-
  % Open the file for reading, creating a stream along the way.
  open(File, read, Stream),
  stream_to_atom(Stream, Atom),
  close(Stream).

%% file_type_alternative(+FromFile:atom, +ToFileType:atom, -ToFile:atom) is det.
% Return an alternative of the given file with the given file type.
%
% @param FromFile The atomic name of a file.
% @param ToFileType The atomic name of a file type.
% @param ToFile The atomic name of a file.

file_type_alternative(FromFile, ToFileType, ToFile):-
  file_name_type(Base, _FromFileType, FromFile),
  file_name_type(Base, ToFileType, ToFile).

%% nested_dir_name(+NestedDirectory:compound, -AbsoluteDirectory:atom) is det.
% Returns a nested file path.
%
% @param NestedDirectory A compound term of linearly nested atoms
%        representing the subsequent subdirectories. The final atom
%        is the name of the file.
% @param AbsoluteDirectory The absolute path of the nested directory
%        specification.

nested_dir_name(NestedDirectory, AbsoluteDirectory):-
  NestedDirectory =.. [OuterDirectory | InnerDirectories],
  (
    RelativeDir =.. [OuterDirectory, '.'],
    absolute_file_name(RelativeDir, Path),
    !
  ;
    Path = OuterDirectory
  ),
  create_directory(Path),
  % Make sure that non-nested directories can also be resolved.
  (
    InnerDirectories == []
  ->
    AbsoluteDirectory = Path
  ;
    InnerDirectories = [InnerDirectories0],
    nested_dir_name1(InnerDirectories0, Path, AbsoluteDirectory)
  ).

nested_dir_name1(Directory, Path, AbsoluteDirectory):-
  atomic(Directory),
  !,
  concat_path(Path, Directory, AbsoluteDirectory),
  create_directory(AbsoluteDirectory).
nested_dir_name1(NestedDirectory, Path, AbsoluteDirectory):-
  NestedDirectory =.. [OuterDirectory, InnerDirectories],
  concat_path(Path, OuterDirectory, NewPath),
  create_directory(NewPath),
  nested_dir_name1(InnerDirectories, NewPath, AbsoluteDirectory).

%% path_walk_forest(
%%   +Directories:list(atom),
%%   +RegularExpression:atom,
%%   -AbsoluteFileNames:list(atom)
%% ) is det.
% Returns the absolute paths of all files that are in the given directories.
% Search recurses throught subdirectories.
%
% @param Directories A list of directory names.
% @param RegularExpression A regular expression filter on file search.
% @param AbsoluteFileNames A list of absolute file names.

path_walk_forest([], _RegularExpression, []).
path_walk_forest(
  [Directory | Directories],
  RegularExpression,
  AbsoluteFileNames
):-
  path_walk_tree(Directory, RegularExpression, AbsoluteFileNames1),
  path_walk_forest(Directories, RegularExpression, AbsoluteFileNames2),
  append(AbsoluteFileNames1, AbsoluteFileNames2, AbsoluteFileNames).

%% path_walk_tree(
%%  +MainDir:atom,
%%  +RE:atom,
%%  -Paths:list(atom)
%% ) is det.
% Returns the file paths in the given directory answering the given regular
% expression. The directories are searched recursively.
%
% @param MainDir A directory path.
% @param RE A regular expression filter.
%        Example: =|'.*.pl$'|=
% @param Paths A list of absolute file paths.

path_walk_tree(MainDir, RE, AbsoluteFileNames1):-
  % Find all relative file names.
  new(RelativeFileNamesChain, chain),
  new(RelativeSubDirsChain, chain),
  send(directory(MainDir), scan,
    files := RelativeFileNamesChain,
    directories := RelativeSubDirsChain,
    hidden_too := @off,
    pattern := regex(RE)
  ),

  % Turn all relative file names into absolute file names.
  chain_list(RelativeFileNamesChain, RelativeFileNames),
  findall(
    AbsoluteFileName,
    (
      member(RelativeFileName, RelativeFileNames),
      absolute_file_name(
        RelativeFileName,
        AbsoluteFileName,
        [relative_to(MainDir)]
      )
    ),
    AbsoluteFileNames
  ),

  % Turn all relative subdirectories into absolute subdirectories.
  chain_list(RelativeSubDirsChain, RelativeSubDirs),
  findall(
    AbsoluteSubDir,
    (
      member(RelativeSubDir, RelativeSubDirs),
      format(atom(AbsoluteSubDir), '~w/~w', [MainDir, RelativeSubDir])
    ),
    AbsoluteSubDirs
  ),

  % Traverse the subdirectories recursively.
  path_walk_forest(AbsoluteSubDirs, RE, RecursiveAbsoluteFileNames),

  % Combine the results.
  append(AbsoluteFileNames, RecursiveAbsoluteFileNames, AbsoluteFileNames1).

%% read_terms(+Stream:stream, -Terms:list(term), +Options:list(atom)) is det.
% Returns the terms as they occur on the given stream.
%
% @param Stream
% @param Terms
% @param Options

read_terms(Stream, Terms, Options):-
  read_term(Stream, Term, Options),
  read_terms_(Stream, Term, Terms, Options).

read_terms_(_Stream, end_of_file, [], _Options):-
  !.
read_terms_(Stream, Term, [Term | Terms], Options):-
  read_terms(Stream, Terms, Options).

stream_to_atom(Stream, Atom):-
  % First we convert to 'codes', and then to an atom.
  read_stream_to_codes(Stream, Codes),
  % An atom is enough like a string to be used in string concatenation etc.
  atom_codes(Atom, Codes).

stream_to_file(Stream, File):-
  stream_to_atom(Stream, Atom),
  atom_to_file(Atom, File).

