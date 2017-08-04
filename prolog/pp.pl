:- module(
  pp,
  [
    pp_term/1,   % +Term
    pp_term_nl/1 % +Term
  ]
).
:- reexport(library(pprint)).

/** <module> Pretty-print

@author Wouter Beek
@version 2017/07
*/





%! pp_term(+Term:term) is det.

pp_term(Term) :-
  print_term(Term, []).



%! pp_term_nl(+Term:term) is det.

pp_term_nl(Term) :-
  pp_term(Term),
  nl.
