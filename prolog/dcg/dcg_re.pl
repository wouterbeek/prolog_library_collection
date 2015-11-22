:- module(
  dcg_re,
  [
    ?//2, % :Dcg_1
          % -Content:list
    *//2, % :Dcg_1
          % -Content:list
    +//2, % :Dcg_1
          % -Content:list
    'm*n'//4, % +Low:nonneg
              % +High:nonneg
              % :Dcg_1
              % -Content:list
    posfrac/2, % +Digits:list(between(0,9))
               % -FractionalPart:rational
    possum/2, % +Digits, -Number
    possum/3 % +Digits:list(nonneg)
             % +Base:positive_integer
             % -Number:nonneg
  ]
).

/** <module> DCG: Regular Expression

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/rfc2234)).
:- use_module(library(lists)).

:- meta_predicate(?(//,?,?)).
:- meta_predicate(?(3,-,?,?)).
:- meta_predicate(*(//,?,?)).
:- meta_predicate(*(3,-,?,?)).
:- meta_predicate(+(//,?,?)).
:- meta_predicate(+(3,-,?,?)).
:- meta_predicate(#(+,//,?,?)).
:- meta_predicate(#(+,3,-,?,?)).
:- meta_predicate('m*n'(?,?,//,?,?)).
:- meta_predicate('m*n'(?,?,3,-,?,?)).
:- meta_predicate('m*n__'(?,?,//,-,?,?)).
:- meta_predicate('m*n__'(?,?,+,3,-,?,?)).




?(Dcg_1) --> 'm*n'(0, 1, Dcg_1).

?(Dcg_1, L) --> 'm*n'(0, 1, Dcg_1, L).

*(Dcg_1) --> 'm*n'(0, _, Dcg_1).

*(Dcg_1, L) --> 'm*n'(0, _, Dcg_1, L).

#(N, Dcg_1) --> 'm*n'(N, N, Dcg_1).

#(N, Dcg_1, L) --> 'm*n'(N, N, Dcg_1, L).

+(Dcg_1) --> 'm*n'(1, _, Dcg_1).

+(Dcg_1, L) --> 'm*n'(1, _, Dcg_1, L).



%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1)// .

'm*n'(Low, High, Dcg_1) --> 'm*n__'(M, N, 0, Dcg_1).

'm*n__'(M, N, C1, Dcg_1) -->
  {(var(N) -> true ; C < N)},
  Dcg_1, !,
  {C2 is C1 + 1},
  'm*n__'(M, N, C2, Dcg_1).
'm*n__'(M, _, C, _) --> {(var(M) -> true ; M =< C)}.


%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1, -Content:list)// .

'm*n'(M, N, Dcg_1, L) --> 'm*n__'(M, N, 0, Dcg_1, L).

'm*n__'(M, N, C1, Dcg_1, [H|T]) -->
  {(var(N) -> true ; C < N)},
  dcg_call(Dcg_1, H), !,
  {C2 is C1 + 1},
  'm*n__'(M, N, C2, Dcg_1, T).
'm*n__'(M, _, C, _, []) --> {(var(M) -> true ; M =< C)}.



%! posfrac(+Digits:list(between(0,9)), -FractionalPart:rational) is det.

postfrac(Ds, Frac):-
  aggregate_all(
    sum(N),
    (
      nth1(I, Ws, W),
      N is W rdiv (10 ^ I)
    ),
    F
  ).



possum(L, Base, N):- possum(L, Base, 0, N).
possum([H|T], Base, N1, N):- !,
  N2 is N1 * Base + H,
  possum(T, Base, N2, N).
possum([], _, N, N).
