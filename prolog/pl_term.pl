:- module(
  pl_term,
  [
    write_fact/1, % @Term
    write_term/1 % @Term
  ]
).

/** <module> Prolog term

Additional support for handling Prolog terms.

@author Wouter Beek
@version 2015/07, 2015/10
*/

:- use_module(library(apply)).





%! write_fact(+Fact:compound) is det.

write_fact(Term) :-
  write_term(Term),
  write(.),
  nl.



%! write_term(@Term) is det.
% Alternative to write_canonical/[1,2] that lives up to the promise that
% "terms written with this predicate can always be read back".

write_term(Term) :-
  replace_blobs(Term, AtomBlobs),
  write_term(AtomBlobs, [numbervars(true),quoted(true)]).


%! replace_blobs(Term0, Term) is det.
% Copy Term0 to Term, replacing non-text blobs.
% This is required for error messages that may hold streams
% and other handles to non-readable objects.

replace_blobs([], []) :- !.
replace_blobs(Blob, Atom) :-
  blob(Blob, Type),
  Type \== text, !,
  format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term) :-
  compound(Term0), !,
  compound_name_arguments(Term0, Pred, Args0),
  maplist(replace_blobs, Args0, Args),
  compound_name_arguments(Term, Pred, Args).
replace_blobs(Term, Term).
