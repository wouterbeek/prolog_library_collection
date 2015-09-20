:- module(
  nlp,
  [
    plural//2 % +Count:integer
              % +Singular:atom
  ]
).

/** <module> Natural Language Processing extensions

Simple NLP predicates for often occurring cases.

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(dcg/basics)).





%! plural(+Count:integer, +Singular:atom)// is det.

plural(C, N) -->
  atom(N),
  ({abs(C) =:= 1} -> "" ; "s").
