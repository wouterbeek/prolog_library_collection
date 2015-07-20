:- module(pl_clause_enhancement, []).

/** <module> Prolog clause enhancement

Automatic clause enhancement in SWI-Prolog,
enforcing determinism and/or performing typechecks.

We assume a one-to-one mapping between mode lines an clause heads.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(lists), except([delete/3,subset/2])).

:- use_module(plc(prolog/pl_determinism)).
:- use_module(plc(prolog/pl_typecheck)).





%! pl_clause_enhancement(
%!   +Typecheck:boolean,
%!   +Determinism:boolean,
%!   +Clause:compound,
%!   -Clauses:list(compound)
%! ) is det.

pl_clause_enhancement(Typecheck, Determinism, Clause, Clauses):-
  % Clause enhancement cannot be applied to this very module.
  prolog_load_context(module, Module),
  \+ member(Module, [pl_clause_enhancement,pl_determinism,pl_typecheck]),

  % Add typechecking.
  (   Typecheck == true
  ->  add_typecheck(Clause, TypecheckedClause)
  ;   TypecheckedClause = Clause
  ),

  % Add determinism.
  (   Determinism == true
  ->  add_determinism(TypecheckedClause, Clauses)
  ;   Clauses = [TypecheckedClause]
  ).

user:term_expansion(Clause, Clauses):-
  pl_clause_enhancement(true, true, Clause, Clauses).
