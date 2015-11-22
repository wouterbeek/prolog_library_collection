:- module(
  rfc2234_re,
  [
    '*DIGIT'//1, % ?Weight:nonneg
    '+DIGIT'//1, % ?Weight:nonneg
    '#DIGIT'//2, % +Occurrences:nonneg
                 % ?Weight:nonneg
    '*HEXDIG'//1, % ?Weight:nonneg
    '+HEXDIG'//1 % ?Weight:nonneg
  ]
).

/** <module> RFC 2234: Regular Expressions

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).
:- use_module(library(dcg/rfc2234)).





'*DIGIT'(I) --> *('DIGIT', L), {possum(L, I)}.

'+DIGIT'(I) --> +('DIGIT', L), {possum(L, I)}.

'#DIGIT'(M, I) --> #(M, 'DIGIT', L), {possum(L, I)}.

'*HEXDIG'(I) --> *('HEXDIG', L), {possum(L, I)}.

'+HEXDIG'(I) --> +('HEXDIG', L), {possum(L, I)}.

'm*nHEXDIG'(M, N, I) --> 'm*n'(M, N, 'HEXDIG', L), {possum(L, I)}.
