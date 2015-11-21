:- module(
  dcg_cardinal,
  [
    between//2, % +Low:integer
                % +High:integer
    between//3, % +Low:integer
                % +High:integer
                % ?Value:integer
    between_digit//2, % +Low:hex
                      % +High:hex
    between_digit//3, % +Low:hex
                      % +High:hex
                      % -Weight:between(0,15)
    between_digit//4, % +Low:hex
                      % +High:hex
                      % -Weight:between(0,15)
                      % -Code:code
    between_radix//2, % +Low:compound
                      % +High:compound
    between_radix//3, % +Low:compound
                      % +High:compound
                      % ?Value:compound
    thousands_integer//1 % +Integer:integer
  ]
).
:- reexport(library(dcg/basics)).

/** <module> Processing cardinal numbers in DCGs

Library `dcg/basics` comes with the following rules for cardinals:
  - `float//1`
  - `integer//1`
  - `number//1`

---

@author Wouter Beek
@version 2015/07-2015/08
*/

:- use_module(library(error)).
:- use_module(library(plunit)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(math/math_ext)).
:- use_module(library(math/radconv)).
:- use_module(library(math/radix)).
:- use_module(library(typecheck)).





%! between(+Low:integer, +High:integer)// .
%! between(+Low:integer, +High:integer, ?Value:integer)// .
% Consume integers between the given lower and higher bounds.

between(Low, High) --> between(Low, High, _).

between(Low, High, N) --> integer(N), {between(Low, High, N)}.



%! between_digit(+LowWeight:hex, +HighWeight:hex)// .
%! between_digit(+LowWeight:hex, +HighWeight:hex, -Weight:between(0,15))// .
%! between_digit(
%!   +LowWeight:hex,
%!   +HighWeight:hex,
%!   -Weight:between(0,15),
%!   -Code:code
%! )// .
% Consume digits between the given lower and higher bounds.
%
% This supports digits of hexadecimal radix.

between_digit(Low, High) --> between_digit(Low, High, _).

between_digit(Low, High, W) --> between_digit(Low, High, W, _).

between_digit(Low, High, W, C) -->
  hexadecimal_digit(W, C),
  {between(Low, High, W)}.



%! between_radix(+Low:compound, +High:compound)// .
%! between_radix(+Low:compound, +High:compound, ?Value:compound)// .
% Consume integers that are specified in various bases
% (binary, octal, decimal, hexadecimal)
% and that are between the lower and higher bounds.
%
% ### Example
%
% ```prolog
% ?- phrase(between_radix(bin(1001), hex(f), oct(X)), Codes).
% X = 11,
% Codes = [57] ;
% X = 12,
% Codes = [49, 48] ;
% X = 13,
% Codes = [49, 49] ;
% X = 14,
% Codes = [49, 50] ;
% X = 15,
% Codes = [49, 51] ;
% X = 16,
% Codes = [49, 52] ;
% X = 17,
% Codes = [49, 53].
% ```

between_radix(Low, High) -->
  between_radix(Low, High, _).

between_radix(Low, High, Value) -->
  {
    radconv(Low, dec(LowDec)),
    radconv(High, dec(HighDec)),
    between(LowDec, HighDec, ValueDec)
  },
  integer(ValueDec),
  {radconv(dec(ValueDec), Value)}.




%! thousands_integer(+Integer:integer)// is det.

thousands_integer(Integer) -->
  {format(atom(Atom), '~D', [Integer])},
  atom(Atom).
