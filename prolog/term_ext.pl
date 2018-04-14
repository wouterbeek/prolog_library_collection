:- module(
  term_ext,
  [
    groundness/3,              % -Order, +Term1, +Term2
    groundsort/2,              % +Terms, -SortedTerms
    predmsort/3,               % :Compare_2, +Terms, -SortedTerms
    term_number_of_variables/2 % +Term, -NumberOfVariables
  ]
).

/** <module> Extensions for working with Prolog terms

@author Wouter Beek
@version 2018
*/

:- use_module(library(apply)).
:- use_module(library(error)).

:- meta_predicate
    predmerge(3, +, +, -),
    predmerge(+, 3, +, +, -, -, -),
    predmsort(3, +, -),
    predmsort(3, ?, +, ?, -).





%! groundness(-Order:oneof([<,=,>]), +Term1:term, +Term2:term) is det.

groundness(Order, Term1, Term2) :-
  maplist(term_number_of_variables, [Term1,Term2], [N1,N2]),
  compare(Order, N1, N2).



%! groundsort(+Terms:list(term), -SortedTerms:list(term)) is det.

groundsort(L1, L2) :-
  predmsort(groundness, L1, L2).



%! predmsort(:Compare_2, +L1:list, -L2:list) is det.
%
% predsort/3 using msort/2.

predmsort(P, L, R) :-
  '$skip_list'(N, L, Tail),
  (Tail == [] -> predmsort(P, N, L, _, R1), R = R1 ; must_be(L, list)).

predmsort(P, 2, [X1,X2|L], L, R) :- !,
  call(P, Delta, X1, X2),
  msort_(Delta, X1, X2, R).
predmsort(_, 1, [X|L], L, [X]) :- !.
predmsort(_, 0, L, L, []) :- !.
predmsort(P, N, L1, L3, R) :-
  N1 is N // 2,
  plus(N1, N2, N),
  predmsort(P, N1, L1, L2, R1),
  predmsort(P, N2, L2, L3, R2),
  predmerge(P, R1, R2, R).

msort_(<, X1, X2, [X1,X2]).
msort_(=, X1, X2, [X1,X2]).
msort_(>, X1, X2, [X2,X1]).

predmerge(_, [], R, R) :- !.
predmerge(_, R, [], R) :- !.
predmerge(P, [H1|T1], [H2|T2], Result) :-
  call(P, Delta, H1, H2), !,
  predmerge(Delta, P, H1, H2, T1, T2, Result).

predmerge(>, P, H1, H2, T1, T2, [H2|R]) :-
  predmerge(P, [H1|T1], T2, R).
predmerge(=, P, H1, H2, T1, T2, [H1,H2|R]) :-
  predmerge(P, T1, T2, R).
predmerge(<, P, H1, H2, T1, T2, [H1|R]) :-
  predmerge(P, T1, [H2|T2], R).



%! term_number_of_variables(+Term:term, -NumberOfVariables:nonneg) is det.

term_number_of_variables(Term, N) :-
  term_variables(Term, Vars),
  length(Vars, N).
