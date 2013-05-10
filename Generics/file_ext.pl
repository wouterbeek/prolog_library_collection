:- module(
  file_ext,
  [
    atom_to_file/2, % +Atom:atom
                    % +File:atom
    base_or_file_to_file/3, % +BaseOrFile:atom
                            % ?FileType:atom
                            % -File:atom
    compiled_file/2, % +PL_File:atom
                     % -QLF_File:atom
    create_directory/1, % +Dir:atom
    create_file/4, % +NestedDir:term
                   % +Name:atom
                   % +Type:atom
                   % -File:atom
    file_name/4, % +File:atom
                 % ?Dir:atom
                 % ?Name:atom
                 % ?Ext:atom
    file_name_type/3, % ?Base:atom
                      % ?Type:atom
                      % ?Name:atom
    file_to_atom/2, % +File:file
                    % -Atom:atom
    file_type_alternative/3, % +FromFile:atom
                             % +ToFileType:atom
                             % -ToFile:atom
    nested_dir_name/2, % +NestedDirs:compound
                       % -Dir:atom
    nested_dir_name/3, % +NestedDirs:compound
                       % +OldDir:atom
                       % -NewDir:atom
    path_walk_forest/3, % +Dirs:list(atom)
                        % +RE:atom
                        % -AbsoluteFileNames:list(atom)
    path_walk_tree/3, % +RootDir:file
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

---+ Abbreviations

We use the following abbreviations in this module:
  * Dir
    Directory
  * Ext
    Extension
  * PL
    Prolog
  * QLF
    QuickLoadFormat
  * RE
    RegularExpression

@author Wouter Beek
@version 2011/08-2012/05, 2012/09, 2013/04-2013/05
*/

:- use_module(library(debug)).

:- nodebug(file_ext).



%% atom_to_file(+Atom:atom, -File:atom) is det.
% Stores the given atom in the given file.
%
% @param Atom An atom.
% @param File An atomic file name.

atom_to_file(Atom, File):-
  access_file(File, write),
  open(File, write, Stream),
  format(Stream, '~w', [Atom]),
  close(Stream).

%% base_or_file_to_file(
%%   +BaseOrFile:atom,
%%   +FileType:atom,
%%   -File:atom
%% ) is semidet.
% Predicates that take file arguments can use this to allow either
% absolute file names or file base names to be accepted.
%
% This is useful when there are multiple file extensions associated with
% the same file type and the calling predicate only looks at the file type
% level.
%
% @param BaseOrFile Either a full file name or the base name of a file.
%        In the former case we check for a supported file extension.
%        In the latter case we add a supported file extension.
% @param FileType The atomic name of a registered file type.
% @param File An absolute file name.

base_or_file_to_file(BaseOrFile, FileType, File):-
  (
    file_name_type(_Base, FileType, BaseOrFile)
  ->
    File = BaseOrFile
  ;
    file_name_type(BaseOrFile, FileType, File)
  ),
  access_file(File, read),
  % Since there may be multiple file type / file extension translations,
  % the above may backtrack. Therefore we discard these choice-points here.
  % I.e., we only use the first file we find.
  !.

%% compiled_file(PL_File, QLF_File) is det.
% Returns the compiled version of the given Prolog file.
%
% Compiled files are called Quick Load Files (QLF).
%
% This method checks whether the Prolog file needs to be recompiled,
% or whether a previously compiled QLF file can be used instead.
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

%% create_directory(+Dir:atom) is det.
% Creates a directory with the given name.
%
% @param Dir The atomic name of a directory.

% The directory already exists, so do nothing.
create_directory(Dir):-
  exists_directory(Dir),
  !.
% The directory does not already exist, so create it.
create_directory(Dir):-
  make_directory(Dir).

%% create_file(+NestedDir:term, +Name:atom, +Type:atom, -File:atom) is det.
% Creates a file with the given name, inside the given directory, and that
% is of the given file type.
%
% File types are resolved using prolog_file_type/2.
%
% @param NestedDir The atomic name of a directory or a compound term that
%        can be resolved by subsequent applications of absolute_file_name/[2,3],
%        e.g. =|aaa(bbb(ccc))|=.
% @param Base The atomic base name of a file.
% @param Type The atomic name of a file type, as registered with
%        prolog_file_type/2, e.g. =|mp3|=.
% @param File The atomic absolute name of a file.

create_file(NestedDir, Base, Type, File):-
  % Resolve the directory in case the compound term notation employed
  % by absolute_file_name/3 is used.
  (
    compound(NestedDir)
  ->
    absolute_file_name(NestedDir, Dir)
  ;
    Dir = NestedDir
  ),

  % Make sure that the directory exists.
  create_directory(Dir),

  % Create the local file name by appending the base and extension names.
  % The extension must be of the given type.
  file_name_type(Base, Type, Local),

  % Append directory and file name.
  format(atom(File), '~w/~w', [Dir, Local]).

%% file_name(+File:atom, ?Dir:atom, ?Base:atom, ?Ext:atom) is semidet.
%% file_name(-File:atom, +Dir:atom, +Base:atom, +Ext:atom) is semidet.
% The splitting of a file into its directory, local name and type parts.

file_name(File, Dir, Base, Ext):-
  nonvar(File),
  !,
  file_directory_name(File, Dir),
  file_base_name(File, Local),
  file_name_extension(Base, Ext, Local).
file_name(File, Dir, Base, Ext):-
  var(File),
  maplist(nonvar, [Dir, Base, Ext]),
  !,
  file_name_extension(Base, Ext, Local),
  absolute_file_name(Local, File, [relative_to(Dir)]).

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
  user:prolog_file_type(Ext, Type),
  file_name_extension(Name, Ext, File).
file_name_type(Name, Type, File):-
  nonvar(File),
  !,
  file_name_extension(Name, Ext, File),
  user:prolog_file_type(Ext, Type).

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

%% file_type_alternative(
%%   +FromFile:atom,
%%   +ToFileType:atom,
%%   -ToFile:atom
%% ) is det.
% Returns an alternative of the given file with the given file type.
%
% @param FromFile The atomic name of a file.
% @param ToFileType The atomic name of a file type.
% @param ToFile The atomic name of a file.

file_type_alternative(FromFile, ToFileType, ToFile):-
  file_name_type(Base, _FromFileType, FromFile),
  file_name_type(Base, ToFileType, ToFile).

%% nested_dir_name(+NestedDir:compound, -Dir:atom) is det.
% Returns a nested file path.
%
% @param NestedDir A compound term of linearly nested atoms
%        representing the subsequent subdirectories. The final atom
%        is the name of the file.
% @param Dir The absolute path of the nested directory specification.

nested_dir_name(NestedDir, Dir):-
  atomic(NestedDir),
  !,
  Spec =.. [NestedDir, '.'],
  absolute_file_name(Spec, Dir),
  create_directory(Dir).
nested_dir_name(NestedDir, Dir):-
  % First we construct the atomic name of the outer directory.
  NestedDir =.. [OuterDir, InnerNestedDir],
  nested_dir_name(OuterDir, OuterDirAtom),
  % Then we add the inner directories recursively.
  nested_dir_name(InnerNestedDir, OuterDirAtom, Dir).

%% nested_dir_name(+NestedDir:term, +OldDir:atom, -NewDir:atom) is det.
% Adds the nested directories term to the given atomic directory,
% returning another atomic directory.

nested_dir_name(SubDir, OldDir, NewDir):-
  atomic(SubDir),
  !,
  absolute_file_name(SubDir, NewDir, [file_type(directory), relative_to(OldDir)]),
  create_directory(NewDir).
nested_dir_name(NestedDir, OldDir, NewDir):-
  NestedDir =.. [OuterDir, InnerNestedDir],
  absolute_file_name(OuterDir, TempDir, [file_type(directory), relative_to(OldDir)]),
  create_directory(TempDir),
  nested_dir_name(InnerNestedDir, TempDir, NewDir).

%% path_walk_forest(
%%   +Dirs:list(atom),
%%   +RE:atom,
%%   -AbsoluteFileNames:list(atom)
%% ) is det.
% Returns the absolute paths of all files that are in the given directories.
% Search recurses through subdirectories.
%
% @param Dirs A list of directory names.
% @param RE A regular expression filter on file search.
% @param AbsoluteFileNames A list of absolute file names.

path_walk_forest([], _RE, []).
path_walk_forest([Dir | Dirs], RE, AbsoluteFileNames):-
  path_walk_tree(Dir, RE, AbsoluteFileNames1),
  path_walk_forest(Dirs, RE, AbsoluteFileNames2),
  append(AbsoluteFileNames1, AbsoluteFileNames2, AbsoluteFileNames).

%% path_walk_tree(+MainDir:atom, +RE:atom, -Paths:list(atom)) is det.
% Returns the file paths in the given directory answering the given regular
% expression. The directories are searched recursively.
%
% @param MainDir A directory path.
% @param RE A regular expression filter. Example: =|'.*.pl$'|=
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
  read_terms0(Stream, Term, Terms, Options).

read_terms0(_Stream, end_of_file, [], _Options):-
  !.
read_terms0(Stream, Term, [Term | Terms], Options):-
  read_terms(Stream, Terms, Options).

%% stream_to_atom(+Stream:stream, -Content:atom) is det.
% Stores the contents of an atom stream to an atom.

stream_to_atom(Stream, Atom):-
  % First we convert to 'codes', and then to an atom.
  read_stream_to_codes(Stream, Codes),
  % An atom is enough like a string to be used in string concatenation etc.
  atom_codes(Atom, Codes).

%% stream_to_file(+Stream:stream, +File:atom) is det.
% Stores an atomic stream to the given file.

stream_to_file(Stream, File):-
  stream_to_atom(Stream, Atom),
  atom_to_file(Atom, File).
