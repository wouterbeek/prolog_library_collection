:- module(
  roman_numerals,
  [
    roman_to_arabic//1 % -Arabic:integer
  ]
).

/** <module> ROMAN_NUMERALS

Predicates supporting the Roman notation for numerals.

@author Wouter Beek
@version 2013/06
*/

:- use_module(dcg(dcg_ascii)).
:- use_module(generics(meta_ext)).



% The meaning of a Roman numeral is context-dependent
% (look at the immediate right).
roman_to_arabic(Z, [X1,Y1|L1], L2):-
  roman_to_arabic(Q, [Y1|L1], L2),
  phrase(roman_to_arabic(X2), [X1]),
  phrase(roman_to_arabic(Y2), [Y1]),
  if_then_else(
    X2 < Y2,
    Z is Q - X2,
    Z is Q + X2
  ).
% Case insensitive.
roman_to_arabic(1) --> i.
roman_to_arabic(5) --> v.
roman_to_arabic(10) --> x.
roman_to_arabic(50) --> l.
roman_to_arabic(100) --> c.
roman_to_arabic(500) --> d.
roman_to_arabic(1000) --> m.

