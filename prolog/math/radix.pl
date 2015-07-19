:- module(
  radix,
  [
    digit_weight/2, % ?Digit:hex
                    % ?Weight:between(0,15)
    is_hex/1 % @Term
  ]
).

/** <module> Radix

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(error)).

:- multifile(error:has_type/2).
error:has_type(hex, Value):-
  error:has_type(or([between(0,9),oneof([a,b,c,d,e,f])]), Value).





%! digit_weight(+Digit:hex, -Weight:between(0,15)) is det.
%! digit_weight(-Digit:hex, +Weight:between(0,15)) is det.

digit_weight(D, D):-
  between(0, 9, D).
digit_weight(a, 10).
digit_weight(b, 11).
digit_weight(c, 12).
digit_weight(d, 13).
digit_weight(e, 14).
digit_weight(f, 15).



%! is_hex(@Term) is semidet.

is_hex(X):-
  digit_weight(X, _).
