:- module(
  hash,
  [
    md5/2 % @Term
          % -Hash:atom
  ]
).

/** <module> Hash

Support for hash functions.

@author Wouter Beek
@version 2015/05
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).



%! md5(@Term, -Hash:atom) is det.

md5(Term, Hash):-
  term_to_atom(Term, Atom),
  rdf_atom_md5(Atom, 1, Hash).
