:- module(
  hash_ext,
  [
    md5/2 % @Term
          % -Hash:atom
  ]
).

/** <module> Hash extensions

Extended support for using hashes.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(semweb/rdf_db)).



%! md5(@Term, -Hash:atom) is det.

md5(Term, Hash):-
  term_to_atom(Term, Atom),
  rdf_atom_md5(Atom, 1, Hash).
