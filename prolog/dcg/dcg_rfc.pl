:- module(
  dcg_rfc,
  [
    alphadigit_hyphen//1, % ?Code:code
    'HEXDIG'//1, % ?Integer:between(0,15)
    'HEXDIG'//2, % ?Integer:between(0,15)
                 % ?Code:code
    '*HEXDIG'//1, % ?Integer:nonneg
    '+HEXDIG'//1, % ?Integer:nonneg
    'm*nHEXDIG'//3 % ?Low:nonneg
                   % ?High:nonneg
                   % ?Integer:nonneg
  ]
).

/** <module> DCG: RFC support

DCGs that I do not particularly like but that are often used in RFCs.

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).





alphadigit_hyphen(C)   --> alphadigit(C).
alphadigit_hyphen(0'-) --> "-".



%! 'HEXDIG'(?Integer:between(0,15))// .
% Wraper around 'HEXDIG'//2.

'HEXDIG'(I) --> 'HEXDIG'(I, _).


%! 'HEXDIG'(?Integer:between(0,15), ?Code:code)// .
% Uppercase-only notation for hexadecimal digits.
%
% ```abnf
% HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ```

'HEXDIG'(I, C)    --> digit(I, C).
'HEXDIG'(10, 0'A) --> "A".
'HEXDIG'(11, 0'B) --> "B".
'HEXDIG'(12, 0'C) --> "C".
'HEXDIG'(13, 0'D) --> "D".
'HEXDIG'(14, 0'E) --> "E".
'HEXDIG'(15, 0'F) --> "F".



'*HEXDIG'(I) --> *('HEXDIG', Ds), {pos_sum(Ds, I)}.

'+HEXDIG'(I) --> +('HEXDIG', Ds), {pos_sum(Ds, I)}.

'm*nHEXDIG'(Low, High, I) --> 'm*n'(Low, High, 'HEXDIG', Ds), {pos_sum(Ds, I)}.
