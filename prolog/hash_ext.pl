:- module(
  hash_ext,
  [
    md5/2 % @Term, -Hash
  ]
).

/** <module> Hash extensions

Extended support for using hashes.

@author Wouter Beek
@version 2015/08, 2016/01
*/

:- use_module(library(semweb/rdf_db), [rdf_atom_md5/3]).



%! md5(@Term, -Hash:atom) is det.

md5(Term, Hash):-
  term_to_atom(Term, Atom),
  rdf_atom_md5(Atom, 1, Hash).
