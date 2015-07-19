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

/** <module> Processing cardinal numbers in DCGs

Library `dcg/basics` comes with the following rules for cardinals:
  - `float//1`
  - `integer//1`
  - `number//1`

Sometimes we want to process integers between a given lower and upper bound:

  - between//[2,3]
    Processes integers between the given lower and higher bounds.
  - between_digit//[2,3]
    Processes digits between the given lower and higher bounds.
    `hex` is defined as `or([between(0,9),oneof([a,b,c,d,e,f])])`.
  - between_radix//[2,3]
    The values are either integers (in decimal base) or compound terms
    (`bin/1`, `oct/1`, `dec/1`, `hex/1`)
    representing numbers in different bases
    (binary, octal, hexadecimal).

### Example:

```prolog
?- phrase(between_radix(bin(1001), hex(f), oct(X)), Codes).
X = 11,
Codes = [57] ;
X = 12,
Codes = [49, 48] ;
X = 13,
Codes = [49, 49] ;
X = 14,
Codes = [49, 50] ;
X = 15,
Codes = [49, 51] ;
X = 16,
Codes = [49, 52] ;
X = 17,
Codes = [49, 53].
```

Besides integers (base 10) we sometimes want to write digits and numbers in
other bases (2, 8, 16).

---

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(error)).
:- use_module(library(plunit)).
:- use_module(library(dcg/basics)).
:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(math/math_ext)).
:- use_module(library(math/radconv)).
:- use_module(library(typecheck)).





%! between(+Low:integer, +High:integer)// .
%! between(+Low:integer, +High:integer, ?Value:integer)// .
% Consume integers between the given lower and higher bounds.

between(Low, High) -->
  between(Low, High, _).

between(Low, High, Value) -->
  integer(Value),
  {between(Low, High, Value)}.



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
%
% @throws type_error If LowWeight or HighWeight are not hexadecimal digits.

between_digit(Low, High) -->
  between_digit(Low, High, _).

between_digit(Low, Hight, Weight) -->
  between_digit(Low, Hight, Weight, _).

between_digit(Low, _, _, _) -->
  {\+ is_hex(Low)}, !,
  {type_error(hex, Low)}.
between_digit(_, High, _, _) -->
  {\+ is_hex(High)}, !,
  {type_error(hex, High)}.
between_digit(Low, High, Weight, Code) -->
  hexadecimal_digit(Weight, Code),
  {between(Low, High, Weight)}.



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
