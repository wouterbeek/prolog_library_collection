:- module(
  rfc_common,
  [
    '*DIGIT'//1, % ?Weight:nonneg
    '+DIGIT'//1, % ?Weight:nonneg
    '#DIGIT'//2 % +Occurrences:nonneg
                % ?Weight:nonneg
  ]
).

/** <module> RFC common rules

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/rfc2234)).





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
  {N2 is N1 + D * 10 ^ M1,
   M2 is M1 - 1},
  '#DIGIT'(M2, N2, N).
