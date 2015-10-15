:- module(
  nlp_grammar,
  [
    plural//2 % +Count:integer
              % +Singular:atom
  ]
).

/** <module> Natural Language: Grammar

Simple NLP predicates for often occurring cases.

@author Wouter Beek
@version 2015/09-2015/10
*/

:- use_module(library(dcg/basics)).





%! plural(+Count:integer, +Singular:atom)// is det.

plural(C, N) -->
  atom(N),
  ({abs(C) =:= 1} -> "" ; "s").
