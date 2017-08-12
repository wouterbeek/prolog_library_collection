:- module(
  hash_ext,
  [
    hash_directory/3, % +Root, +Hash, -Directory
    hash_file/4,      % +Root, +Hash, +Local, -File
    md5/2,            % +Term, -Hash
    md5/3,            % +Term, -Hash, +Options
    sha/2,            % +Term, -Hash
    sha/3             % +Term, -Hash, +Options
  ]
).

/** <module> Hash extensions

Extended support for using hashes.

@author Wouter Beek
@version 2016/07, 2017/08
*/

:- use_module(library(file_ext)).
:- use_module(library(lists)).
:- use_module(library(md5)).
:- use_module(library(sha)).





%! hash_directory(+Root:atom, +Hash:atom, -Directory:atom) is det.

hash_directory(Root, Hash, Dir) :-
  atom_codes(Hash, Codes),
  append([H1,H2], T, Codes),
  atom_codes(Dir1, [H1,H2]),
  atom_codes(Dir2, T),
  append_directories([Root,Dir1,Dir2], Dir).



%! hash_file(+Root:atom, +Hash:atom, +Local:atom, -File:atom) is det.

hash_file(Root, Hash, Local, File) :-
  hash_directory(Root, Hash, Dir),
  create_directory(Dir),
  directory_file_path(Dir, Local, File).



%! md5(+Term, -Hash) is det.
%! md5(+Term, -Hash, +Options) is det.

md5(Term, Hash) :-
  md5(Term, Hash, []).


md5(Term, Hash, Options) :-
  term_to_atom(Term, Atom),
  md5_hash(Atom, Hash, Options).



%! sha(+Term, -Hash) is det.
%! sha(+Term, -Hash, +Options) is det.

sha(Data, Hash) :-
  sha(Data, Hash, []).


sha(Term, Hash, Options) :-
  term_to_atom(Term, Atom),
  sha_hash(Atom, Hash, Options).
