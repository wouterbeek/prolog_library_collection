:- module(
  hash_ext,
  [
    md5/2, % +Term, -Hash
    md5/3, % +Term, -Hash, +Opts
    sha/2, % +Term, -Hash
    sha/3  % +Term, -Hash, +Opts
  ]
).

/** <module> Hash extensions

Extended support for using hashes.

@author Wouter Beek
@version 2016/07
*/

:- use_module(library(md5)).
:- use_module(library(sha)).





%! md5(+Term, -Hash) is det.
%! md5(+Term, -Hash, +Opts) is det.

md5(Term, Hash) :-
  md5(Term, Hash, []).


md5(Term, Hash, Opts) :-
  term_to_atom(Term, A),
  md5_hash(A, Hash, Opts).



%! sha(+Term, -Hash) is det.
%! sha(+Term, -Hash, +Opts) is det.

sha(Data, Hash) :-
  sha(Data, Hash, []).


sha(Term, Hash, Opts) :-
  term_to_atom(Term, A),
  sha_hash(A, Hash, Opts).
