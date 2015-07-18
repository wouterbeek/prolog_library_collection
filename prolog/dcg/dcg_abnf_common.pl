:- module(
  dcg_abnf_common,
  [
    '[-]?'//1, % ?Sign:number
    '[+-]'//1, % ?Sign:number
    '[+-]?'//1, % ?Sign:number
    '[a-z]*'//1, % ?Codes:list(code)
    '[a-z]+'//1, % ?Codes:list(code)
    '[a-zA-Z]*'//1, % ?Codes:list(code)
    '[a-zA-Z]+'//1, % ?Codes:list(code)
    '[0-9]'//1, % ?Digit:between(0,9)
    '[0-9]'//2, % ?Digit:between(0,9)
                % ?Code:code
    '[0-9]*'//1, % ?Digits:list(between(0,9))
    '[0-9]+'//1 % ?Digits:list(between(0,9))
  ]
).

/** <module> DCG: Common ABNF rules

Common ABNF rules that could have been written using dcg_abnf.pl
but that occur often enough to legitimate a dedicated and more efficient
implementation.

@author Wouter Beek
@version 2015
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).





%! '[-]?'(+Sign:number)// .
%! '[-]?'(-Sign:oneof([-1,1]))// .

'[-]?'(Sg) -->
  (   {ground(Sg)}
  ->  (   {Sg < 0}
      ->  "-"
      ;   ""
      )
  ;   (   "-",
          {Sg = -1}
      ;   "",
          {Sg = 1}
      )
  ).



%! '[+-]'(+Sign:number)// .
%! '[+-]'(-Sign:oneof([-1,1]))// .

'[+-]'(Sg) -->
  (   {ground(Sg)}
  ->  (   {Sg < 0}
      ->  "-"
      ;   "+"
      )
  ;   (   "-",
          {Sg = -1}
      ;   "+",
          {Sg = 1}
      )
  ).



%! '[+-]?'(+Sign:number)// .
%! '[+-]?'(-Sign:oneof([-1,1]))// .

'[+-]?'(Sg) -->
  '[+-]'(Sg).
'[+-]?'(1) --> "".



%! '[a-z]*'(?Codes:list(code))// .

'[a-z]*'([H|T]) -->
  ascii_letter_lowercase(H),
  '[a-z]*'(T).
'[a-z]*'([]) --> [].



%! '[a-z]+'(?Codes:list(code))// .

'[a-z]+'([H|T]) -->
  ascii_letter_lowercase(H),
  '[a-z]*'(T).



%! '[a-zA-Z]*'(?Codes:list(code))// .

'[a-zA-Z]*'([H|T]) -->
  ascii_letter(H),
  '[a-zA-Z]*'(T).
'[a-zA-Z]*'([]) --> [].



%! '[a-zA-Z]+'(?Codes:list(code))// .

'[a-zA-Z]+'([H|T]) -->
  ascii_letter(H),
  '[a-zA-Z]*'(T).



%! '[0-9]'(?Weight:between(0,9))// .

'[0-9]'(Weight) -->
  decimal_digit(Weight).

%! '[0-9]'(?Weight:between(0,9), ?Code:code)// .

'[0-9]'(Weight, Code) -->
  decimal_digit(Weight, Code).



%! '[0-9]*'(?Weights:list(between(0,9)))// .

'[0-9]*'([H|T]) -->
  '[0-9]'(H),
  '[0-9]*'(T).
'[0-9]*'([]) --> [].



%! '[0-9]+'(?Weights:list(between(0,9)))// .

'[0-9]+'([H|T]) -->
  '[0-9]'(H),
  '[0-9]*'(T).

