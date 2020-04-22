:- module(
  type_ext,
  [
    positive_integer/1 % @Value
  ]
).

/** <module> Extended support for types

Extends support for types in the SWI-Prolog standard library.

*/

:- use_module(library(error)).





%! positive_integer(@Term) is semidet.

positive_integer(Term) :-
  error:has_type(positive_integer, Term).
