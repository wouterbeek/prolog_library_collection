:- module(
  hash_ext,
  [
    hash_directory/2, % +Hash, -Directory
    hash_directory/3, % +Root, +Hash, -Directory
    hash_file/3,      % +Hash, +Local, -File
    hash_file/4,      % +Root, +Hash, +Local, -File
    md5/2,            % +Term, -Hash
    md5/3,            % +Term, -Hash, +Options
    md5_text/2,       % +Text, -Hash
    md5_text/3,       % +Text, -Hash, +Options
    sha/2,            % +Term, -Hash
    sha/3,            % +Term, -Hash, +Options
    sha_text/2,       % +Text, -Hash
    sha_text/3        % +Text, -Hash, +Options
  ]
).

/** <module> Extended support for hashes

*/

:- use_module(library(lists)).
:- reexport(library(md5), [
     md5_hash/3 as md5_text
   ]).
:- reexport(library(sha), [
     sha_hash/3 as sha_text
   ]).

:- use_module(library(file_ext)).





%! hash_directory(+Hash:atom, -Directory:atom) is det.
%! hash_directory(+Root:atom, +Hash:atom, -Directory:atom) is det.

hash_directory(Hash, Dir2) :-
  working_directory(Root),
  hash_directory(Root, Hash, Dir2).


hash_directory(Root, Hash, Dir2) :-
  sub_atom(Hash, 0, 2, _, Subdir1),
  directory_file_path(Root, Subdir1, Dir1),
  sub_atom(Hash, 2, _, 0, Subdir2),
  directory_file_path(Dir1, Subdir2, Dir2).



%! hash_file(+Hash:atom, +Local:atom, -File:atom) is det.
%! hash_file(+Root:atom, +Hash:atom, +Local:atom, -File:atom) is det.

hash_file(Hash, Local, File) :-
  working_directory(Root),
  hash_file(Root, Hash, Local, File).


hash_file(Root, Hash, Local, File) :-
  hash_directory(Root, Hash, Dir),
  create_directory(Dir),
  directory_file_path(Dir, Local, File).



%! md5(+Term, -Hash:atom) is det.
%! md5(+Term, -Hash:atom, +Options:list(compound)) is det.

md5(Term, Hash) :-
  md5(Term, Hash, []).


md5(Term, Hash, Options) :-
  term_to_atom(Term, Atom),
  md5_hash(Atom, Hash, Options).



%! md5_text(+Text:text, -Hash:atom) is det.
%! md5_text(+Text:text, -Hash:atom, +Options:list(compound)) is det.

md5_text(Text, Hash) :-
  md5_text(Text, Hash, []).



%! sha(+Term, -Hash:atom) is det.
%! sha(+Term, -Hash:atom, +Options:list(compound)) is det.

sha(Data, Hash) :-
  sha(Data, Hash, []).


sha(Term, Hash, Options) :-
  term_to_atom(Term, Atom),
  sha_hash(Atom, Hash, Options).



%! sha_text(+Text:text, -Hash:atom) is det.
%! sha_text(+Text:text, -Hash:atom, +Options:list(compound)) is det.

sha_text(Text, Hash) :-
  sha_text(Text, Hash, []).
