:- module(
  dcg_abnf_common,
  [
    '[-]?'//1, % ?Sign:oneof([-1,1])
    '[+-]'//1, % ?Sign:oneof([-1,1])
    '[+-]?'//1, % ?Sign:oneof([-1,1])
    '[a-z]*'//1, % ?Codes:list(code)
    '[a-z]+'//1, % ?Codes:list(code)
    '[a-zA-Z]*'//1, % ?Codes:list(code)
    '[a-zA-Z]+'//1, % ?Codes:list(code)
    '[0-9]'//1, % ?Weight:between(0,9)
    '[0-9]'//2, % ?Weight:between(0,9)
                % ?Code:code
    '[0-9]*'//1, % ?Weights:list(nonneg)
    '[0-9]+'//1 % ?Weights:list(nonneg)
  ]
).

/** <module> DCG: Common ABNF rules

Common ABNF rules that could have been written using dcg_abnf.pl
but that occur often enough to legitimate a dedicated and more efficient
implementation.

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(dcg/dcg_ascii)).





%! '[-]?'(-Sign:oneof([-1,1]))// .

'[-]?'(-1) --> "-", !.
'[-]?'(1) --> "".



%! '[+-]'(-Sign:oneof([-1,1]))// .

'[+-]'(-1) --> "-", !.
'[+-]'(1)  --> "+".



%! '[+-]?'(-Sign:oneof([-1,1]))// .

'[+-]?'(Sg) --> '[+-]'(Sg), !.
'[+-]?'(1) --> "".



%! '[a-z]*'(?Codes:list(code))// .

'[a-z]*'([H|T]) --> ascii_letter_lowercase(H), '[a-z]*'(T).
'[a-z]*'([]) --> [].



%! '[a-z]+'(?Codes:list(code))// .

'[a-z]+'([H|T]) --> ascii_letter_lowercase(H), '[a-z]*'(T).



%! '[a-zA-Z]*'(?Codes:list(code))// .

'[a-zA-Z]*'([H|T]) --> ascii_letter(H), !, '[a-zA-Z]*'(T).
'[a-zA-Z]*'([]) --> [].



%! '[a-zA-Z]+'(?Codes:list(code))// .

'[a-zA-Z]+'([H|T]) --> ascii_letter(H), '[a-zA-Z]*'(T).



%! '[0-9]'(?Weight:between(0,9))// .

'[0-9]'(W) --> decimal_digit(W).

%! '[0-9]'(?Weight:between(0,9), ?Code:code)// .

'[0-9]'(W, C) --> decimal_digit(W, C).



%! '[0-9]*'(?Weights:list(between(0,9)))// .

'[0-9]*'([H|T]) --> '[0-9]'(H), !, '[0-9]*'(T).
'[0-9]*'([]) --> [].



%! '[0-9]+'(?Weights:list(between(0,9)))// .

'[0-9]+'([H|T]) --> '[0-9]'(H), '[0-9]*'(T).
