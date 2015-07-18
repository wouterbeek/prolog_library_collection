:- module(pl_term, [write_canonical_blobs/1]).

/** <module> Prolog term

Additional support for handling Prolog terms.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).





%! write_canonical_blobs(@Term) is det.
% Alternative to write_canonical/[1,2] that lives up to the promise that
% "terms written with this predicate can always be read back".

write_canonical_blobs(Term):-
  replace_blobs(Term, AtomBlobs),
  write_term(AtomBlobs, [quoted(true)]).

%! replace_blobs(Term0, Term) is det.
% Copy Term0 to Term, replacing non-text blobs.
% This is required for error messages that may hold streams
% and other handles to non-readable objects.

replace_blobs(NIL, NIL):-
  NIL == [], !.
replace_blobs(Blob, Atom):-
  blob(Blob, Type),
  Type \== text, !,
  format(atom(Atom), '~p', [Blob]).
replace_blobs(Term0, Term):-
  compound(Term0), !,
  compound_name_arguments(Term0, Pred, Args0),
  maplist(replace_blobs, Args0, Args),
  compound_name_arguments(Term, Pred, Args).
replace_blobs(Term, Term).
