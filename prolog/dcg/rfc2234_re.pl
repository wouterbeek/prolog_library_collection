:- module(
  rfc2234_re,
  [
    '*DIGIT'//1, % ?Integer:nonneg
    '+DIGIT'//1, % ?Integer:nonneg
    '#DIGIT'//2, % +Occurrences:nonneg
                 % ?Integer:nonneg
    '*HEXDIG'//1, % ?Integer:nonneg
    '+HEXDIG'//1, % ?Integer:nonneg
    'm*nHEXDIG'//3 % ?Low:nonneg
                   % ?High:nonneg
                   % -Integer:nonneg
  ]
).

/** <module> RFC 2234: Regular Expressions

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_re)).
:- use_module(library(dcg/rfc2234)).





'*DIGIT'(I) --> *('DIGIT', Ds), {possum(Ds, I)}.

'+DIGIT'(I) --> +('DIGIT', Ds), {possum(Ds, I)}.

'#DIGIT'(M, I) --> #(M, 'DIGIT', Ds), {possum(Ds, I)}.

'*HEXDIG'(I) --> *('HEXDIG', Ds), {possum(Ds, I)}.

'+HEXDIG'(I) --> +('HEXDIG', Ds), {possum(Ds, I)}.

'm*nHEXDIG'(M, N, I) --> 'm*n'(M, N, 'HEXDIG', Ds), {possum(Ds, I)}.
