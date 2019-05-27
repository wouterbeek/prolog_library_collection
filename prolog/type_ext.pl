:- module(
  type_ext,
  [
    positive_integer/1 % @Value
  ]
).

/** <module> Type extension

@author Wouter Beek
@version 2019
*/

:- use_module(library(error)).





%! positive_integer(@Term) is semidet.

positive_integer(Term) :-
  error:has_type(positive_integer, Term).
