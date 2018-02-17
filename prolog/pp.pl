:- module(
  pp,
  [
    ansi_format/2,   % +Attributes, +String
    call_pp/1,       % :Goal_1
    print_term/1,    % +Term
    print_term_nl/1, % +Term
    print_term_nl/2  % +Term, +Options
  ]
).
:- reexport(library(ansi_term)).
:- reexport(library(pprint)).

/** <module> Pretty-print

@author Wouter Beek
@version 2017-2018
*/

:- meta_predicate
    call_pp(1).





%! ansi_format(+Attributes:list(compound), +String:string) is det.

ansi_format(Attrs, String) :-
  ansi_format(Attrs, String, []).



%! call_pp(:Goal_1) is det.

call_pp(Goal_1) :-
  catch(call(Goal_1, Term), E, true),
  (var(E) -> print_term(Term) ; print_message(warning, E)).



%! print_term(+Term:term) is det.

print_term(Term) :-
  print_term(Term, []).



%! print_term_nl(+Term:term) is det.
%! print_term_nl(+Term:term, +Options:list(compound)) is det.

print_term_nl(Term) :-
  print_term_nl(Term, []).


print_term_nl(Term, Options) :-
  print_term(Term, Options),
  nl.
