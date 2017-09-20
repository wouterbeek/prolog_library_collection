:- module(
  pp,
  [
    call_pp/1,   % :Goal_1
    pp_term/1,   % +Term
    pp_term_nl/1 % +Term
  ]
).
:- reexport(library(pprint)).

/** <module> Pretty-print

@author Wouter Beek
@version 2017/07-2017/09
*/

:- meta_predicate
    call_pp(1).





%! call_pp(:Goal_1) is det.

call_pp(Goal_1) :-
  catch(call(Goal_1, Term), E, true),
  (var(E) -> pp_term(Term) ; print_message(warning, E)).



%! pp_term(+Term:term) is det.

pp_term(Term) :-
  print_term(Term, []).



%! pp_term_nl(+Term:term) is det.

pp_term_nl(Term) :-
  pp_term(Term),
  nl.
