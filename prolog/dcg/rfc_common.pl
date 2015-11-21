:- module(
  rfc_common,
  [
    '*DIGIT'//1, % ?Weight:nonneg
    '+DIGIT'//1, % ?Weight:nonneg
    '#DIGIT'//2, % +Occurrences:nonneg
                 % ?Weight:nonneg
    '*HEXDIG'//1, % ?Weight:nonneg
    '+HEXDIG'//1, % ?Weight:nonneg
    'm*n'//4 % +Low:nonneg
             % +High:nonneg
             % :Dcg_1
             % -Content:list
  ]
).

/** <module> RFC common rules

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/rfc2234)).

:- meta_predicate('m*n'(?,?,3,-,?,?)).
:- meta_predicate('m*n'(?,?,+,3,-,?,?)).





%! '*DIGIT'(?Weight:nonneg)// .
% A non-negative integer serialized as a non-empty sequence of decimal digits.

'*DIGIT'(N) --> '*DIGIT'(0, N).
'*DIGIT'(N1, N) --> 'DIGIT'(D), !, {N2 is N1 * 10 + D}, '*DIGIT'(N2, N).
'*DIGIT'(N, N)  --> "".



%! '+DIGIT'(?Weight:nonneg)// .
% A non-negative integer serialized as a non-empty sequence of decimal digits.

'+DIGIT'(N) --> '+DIGIT'(0, N).
'+DIGIT'(N1, N) --> 'DIGIT'(D), !, {N2 is N1 * 10 + D}, '*DIGIT'(N2, N).



%! '#DIGIT'(+Occurrences:nonneg, -Weights:nonneg)// .
% A non-negative integer that consists of a specified number of digits.

'#DIGIT'(M, N) --> '#DIGIT'(M, 0, N).
'#DIGIT'(0, N, N) --> !, "".
'#DIGIT'(M1, N1, N) -->
  'DIGIT'(D), !,
  {N2 is N1 *10 + D, M2 is M1 - 1},
  '#DIGIT'(M2, N2, N).



%! '*HEXDIG'(?Weight:nonneg)// .
% A non-negative integer serialized as a non-empty sequence of decimal digits.

'*HEXDIG'(N) --> '*HEXDIG'(0, N).
'*HEXDIG'(N1, N) --> 'HEXDIG'(D), !, {N2 is N1 * 10 + D}, '*HEXDIG'(N2, N).
'*HEXDIG'(N, N)  --> "".



%! '+HEXDIG'(?Weight:nonneg)// .
% A non-negative integer serialized as a non-empty sequence of decimal digits.

'+HEXDIG'(N) --> '+HEXDIG'(0, N).
'+HEXDIG'(N1, N) --> 'HEXDIG'(D), !, {N2 is N1 * 10 + D}, '*HEXDIG'(N2, N).



%! 'm*n'(?Low:nonneg, ?High:nonneg, :Dcg_1, -Content:list)// .

'm*n'(M, N, Dcg_1, L) -->
  % Init counter.
  'm*n'(M, N, 0, Dcg_1, L).

'm*n'(M, N, C1, Dcg_1, [H|T]) -->
  % Satisfy higher bound in order to carry on.
  {(var(N) -> true ; C < N)},
  % One more application of DCG rule.
  dcg_call(Dcg_1, H), !,
  % Increment counter.
  {C2 is C1 + 1},
  'm*n'(M, N, C2, Dcg_1, T).
'm*n'(M, _, C, _, []) -->
  % Satisfy lower bound in order to stop.
  {(var(M) -> true ; M =< C)}.
