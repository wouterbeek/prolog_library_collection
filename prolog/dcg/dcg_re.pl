:- module(
  dcg_re,
  [
    '?'//1, % :Dcg_1
    '?'//2, % :Dcg_1
            % -Content:list
    '*'//1, % :Dcg_1
    '*'//2, % :Dcg_1
            % -Content:list
    '+'//1, % :Dcg_1
    '+'//2, % :Dcg_1
            % -Content:list
    '#'//2, % +Occurrences, :Dcg_1
    '#'//3, % +Occurrences:nonneg
            % :Dcg_1
            % -Content:list
    '*n'//3, % ?High:nonneg
             % :Dcg_1
             % -Content:list
    'm*n'//4, % ?Low:nonneg
              % ?High:nonneg
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
:- use_module(library(dcg/dcg_call)).
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
:- meta_predicate('*n'(?,3,-,?,?)).
:- meta_predicate('m*n'(?,?,//,?,?)).
:- meta_predicate('m*n'(?,?,3,-,?,?)).
:- meta_predicate('m*n__'(?,?,+,//,?,?)).
:- meta_predicate('m*n__'(?,?,+,3,-,?,?)).




?(Dcg_1) --> 'm*n'(0, 1, Dcg_1).

?(Dcg_1, L) --> 'm*n'(0, 1, Dcg_1, L).



*(Dcg_1) --> 'm*n'(0, _, Dcg_1).

*(Dcg_1, L) --> 'm*n'(0, _, Dcg_1, L).



#(N, Dcg_1) --> 'm*n'(N, N, Dcg_1).

#(N, Dcg_1, L) --> 'm*n'(N, N, Dcg_1, L).



+(Dcg_1) --> 'm*n'(1, _, Dcg_1).

+(Dcg_1, L) --> 'm*n'(1, _, Dcg_1, L).



%! '*n'(?High:nonneg, :Dcg_1, -Content:list)// .

'*n'(High, Dcg_1, L) --> 'm*n'(_, High, Dcg_1, L).



%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1)// .

'm*n'(Low, High, Dcg_1) --> 'm*n__'(Low, High, 0, Dcg_1).

'm*n__'(Low, High, Count1, Dcg_1) -->
  {(var(High) -> true ; Count1 < High)},
  Dcg_1, !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_1).
'm*n__'(Low, _, Count, _) --> {(var(Low) -> true ; Low =< Count)}.


%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1, -Content:list)// .

'm*n'(Low, High, Dcg_1, L) --> 'm*n__'(Low, High, 0, Dcg_1, L).

'm*n__'(Low, High, Count1, Dcg_1, [H|T]) -->
  {(var(High) -> true ; Count1 < High)},
  dcg_call(Dcg_1, H), !,
  {Count2 is Count1 + 1},
  'm*n__'(Low, High, Count2, Dcg_1, T).
'm*n__'(Low, _, Count, _, []) --> {(var(Low) -> true ; Low =< Count)}.



%! posfrac(+Digits:list(between(0,9)), -FractionalPart:rational) is det.

posfrac(Ds, Frac):-
  aggregate_all(sum(Rat), (nth1(I, Ds, D), Rat is D rdiv (10 ^ I)), Frac).



%! possum(+Digits:list(between(0,9)), -Integer:nonneg) is det.

possum(Ds, I):- possum(Ds, 10, I).


%! possum(
%!   +Digits:list(between(0,9)),
%!   +Base:positive_integer,
%!   -Integer:nonneg
%! ) is det.

possum(Ds, Base, I):- possum(Ds, Base, 0, I).
possum([D|Ds], Base, I1, I):- !, I2 is I1 * Base + D, possum(Ds, Base, I2, I).
possum([], _, I, I).
