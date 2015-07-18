:- module(
  rational_ext,
  [
    is_rational/1, % @Term
    rational_div/3, % +X:compound
                    % +Y:compound
                    % -Z:compound
    rational_float/2, % ?Rational:compound
                      % ?Float:float
    rational_mod/3, % +X:compound
                    % +Y:compound
                    % -Z:compound
    rational_parts/3, % +Decimal:compound
                      % -Integer:integer
                      % -Fractional:compound
    rational_parts_weights/3 % +Decimal:compound
                             % -IntegerWeights:list(between(0,9))
                             % -FractionalWeights:list(between(0,9))
  ]
).

/** <module> Rational number arithmetic extensions

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(apply)).
:- use_module(library(math/positional)).





%! is_rational(@Term) is semidet.

is_rational(T):-
  T = A rdiv B,
  maplist(integer, [A,B]).



%! rational_div(+X:compound, +Y:compound, -Z:compound) is det.
% This is the same as:
% ```prolog
% N is (X1 * Y2) // (Y1 * X2)
% ```
% where
% ```prolog
% X = rdiv(X1,X2)
% Y = rdiv(Y1,Y2)
% ```

rational_div(X, Y, Z):-
  Z is floor(X rdiv Y).



%! rational_float(?Rational:compound, ?Float:float) is det.

rational_float(X, Y):-
  (   float(Y)
  ->  X is rationalize(Y)
  ;   X = X1 rdiv X2,
      Y is X1 / X2
  ).



%! rational_mod(+X:compound, +Y:compound, -Z:compound) is det.

rational_mod(X, Y, Z):-
  rational_div(X, Y, DIV),
  Z is X - DIV * Y.



%! rational_parts(
%!   +Decimal:compound,
%!   -Integer:integer,
%!   -Fraction:compound
%! ) is det.
% ```prolog
% ?- rational_parts(rdiv(111,10), I, F).
% I = 11,
% F = 1 rdiv 10.
%
% ?- rational_parts(rdiv(111,100), I, F).
% I = 1,
% F = 11 rdiv 100.
% ```

rational_parts(D, I, F):-
  rational_div(D, 1, I),
  rational_mod(D, 1, F).



%! rational_parts_weights(
%!   +Decimal:compound,
%!   -IntegerWeights:list(between(0,9)),
%!   -FractionalWeights:list(between(0,9))
%! ) is det.

rational_parts_weights(D, IW, FW):-
  rational_parts(D, I, F),
  positional(I, IW),
  positional_fraction(FW, F).

