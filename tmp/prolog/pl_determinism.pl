:- module(
  pl_determinism,
  [
    add_determinism/2 % +Clause:compound
                      % -DeterminismClause
  ]
).

/** <module> Prolog determinism

Support for enforced semi-determinism in Prolog.

@author Wouter Beek
@version 2014/08
*/

:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).



%! add_determinism(+Clause:compound, -DeterminismClause) is det.

add_determinism(Clause, DeterminismClause):-
  DeterminismClause = Clause.


%! build_mode_clauses(
%!   +Head1:compound,
%!   +Head2:compound,
%!   +ModePairs:list(pair(compound,oneof([det,nondet,multi,semidet]))),
%!   -Clauses:list(compound)
%! ) is det.

build_mode_clauses(_, _, [], []).
build_mode_clauses(Head1, Head2, [ArgDs-Det|ModePairs], Clauses2):-
  build_mode_clauses(Head1, Head2, ModePairs, Clauses1),
  build_mode_clause(Head1, Head2, ArgDs, Det, Clause2),
  (
    member(Clause1, Clauses1),
    unifiable(Clause1, Clause2, _)
  ->
    Clauses2 = Clauses1
  ;
    sort([Clause2|Clauses1], Clauses2)
  ).


%! build_mode_clause(
%!   +Head1:compound,
%!   +Head2:compound,
%!   +ArgumentDescription:list(compound),
%!   +Determinism:oneof([det,nondet,multi,semidet]),
%!   -Clause:compound
%! ) is det.

build_mode_clause(
  Head1,
  Head2,
  ArgD,
  semidet,
  (Head1:- InstantiationCheck, !, Head2, !)
):- !,
  maplist(instantiation_check, ArgD, InstantiationChecks),
  xfy_list(',', InstantiationCheck, InstantiationChecks).
build_mode_clause(Head1, Head2, _, _, (Head1:- Head2)).


%! inner_predicate_head(+Head1:compound, -Head2:compound) is det.

inner_predicate_head(Head1, Head2):-
  Head1 =.. [Pred1|Args],
  atomic_concat(Pred1, '0', Pred2),
  Head2 =.. [Pred2|Args].


%! instantiation_check(
%!   +ArgumentDescription:compound,
%!   -InstantiationCheck:compound
%! ) is det.

instantiation_check(arg('++',X,_), ground(X)).
instantiation_check(arg('+', X,_), nonvar(X)).
instantiation_check(arg('-', X,_), nonground(X)).
instantiation_check(arg('--',X,_), var(X)).


mode_term_to_determinism(is(_,Det), Det):- !.
mode_term_to_determinism(_, nondet).


%! xfy_list(?Op:atom, ?Term, ?List) is det.
% True if elements of List joined together with xfy operator Op gives
% Term. Usable in all directions.
%
% ### Example
%
% ```prolog
% ?- xfy_list(',', (a,b,c), L).
% L = [a, b, c].
%
% ?- xfy_list(Op, 4^3^2, [4,3,2]).
% Op = (^).
% ```

xfy_list(Op, Term, [Left|List]):-
  Term =.. [Op,Left,Right],
  xfy_list(Op, Right, List), !.
xfy_list(_, Term, [Term]).

