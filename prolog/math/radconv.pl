:- module(
  radconv,
  [
    radconv/2 % +From:compound
                % ?To:compound
  ]
).

/** <module> Radix conversion

Converts between numbers in binary, octal, decimal and hexadecimal radix.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(char_ext)).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(typecheck)).





%! radconv(+From:compound, +To:compound) is det.
%! radconv(+From:compound, -To:compound) is det.
% Radix conversion between often used bases.
%
% From and To make use of the followig radix notations:
%   - bin(+nonneg)
%   - dec(+nonneg)
%   - hex(+atom)
%   - oct(+nonneg)
%
% If From is an atom it is assumed to be `hex(+atom)`.
% If From is a non-negative integer it is assumed to be `dec(+nonneg)`.
%
% @throws instantiation_error If both From and To are uninstantiated.

% Swap arguments or
% instantiation error.
radconv(From, To):-
  \+ ground(From), !,
  % One argument has to be ground.
  (   \+ ground(To)
  ->  instantiation_error(To)
  ;   radconv(To, From)
  ).
% Non-radix notation for input: hexadecimal.
radconv(From, To):-
  atom(From), !,
  radconv(hex(From), To).
% Non-radix notation for input: decimal.
radconv(From, To):-
  nonneg(From), !,
  radconv(dec(From), To).
% Radix notation for input: binary, octal, decimal, hexadecimal.
radconv(From, To):-
  From =.. [FromRadix,FromValue], !,
  to_decimal(FromRadix, FromValue, Decimal),
  % If there is no radix for the output value then we assume decimal base.
  (   var(To)
  ->  ToRadix = dec,
      To = dec(ToValue)
  ;   To =.. [ToRadix,ToValue]
  ),
  from_decimal(Decimal, ToRadix, ToValue).





% HELPERS %

%! from_decimal(
%!   +Decimal:nonneg,
%!   +Radix:rad_name,
%!   -Value:or([atom,nonneg])
%! ) is det.

from_decimal(Decimal, dec, Decimal):- !.
from_decimal(Decimal, Radix, Value):-
  radix_value(Radix, RadixValue),
  from_decimal(Decimal, RadixValue, [], Chars),
  radix_chars_to_atomic(Radix, Chars, Value).

from_decimal(0, _, L, L):- !.
from_decimal(N, Base, T, Sol):-
  Mod is N mod Base,
  radix_value(Base, H, Mod),
  Rem is N div Base,
  from_decimal(Rem, Base, [H|T], Sol).



%! radix_chars_to_atomic(
%!   +Radix:rad_name,
%!   +Chars:list(char),
%!   -Value:atomic
%! ) is det.

radix_chars_to_atomic(hex, Chars, Atom):- !,
  atom_chars(Atom, Chars).
radix_chars_to_atomic(Radix, Chars, Value):-
  memberchk(Radix, [bin,oct,dec]),
  number_chars(Value, Chars).



%! radix_value(+Radix:rad_name, -Value:rad_number) is det.

radix_value(bin,  2).
radix_value(dec, 10).
radix_value(hex, 16).
radix_value(oct,  8).



%! radix_value(+Radix:rad_number, +Char:char, +Value:between(0,15)) is semidet.
%! radix_value(+Radix:rad_number, +Char:char, -Value:between(0,15)) is det.
%! radix_value(+Radix:rad_number, -Char:char, +Value:between(0,15)) is det.

radix_value(16, Char, Value):- !,
  char_type(Char, xdigit(Value)).
radix_value(Radix, Char, Value):-
  char_type(Char, digit(Value)),
  Value < Radix.



%! to_decimal(
%!   +Radix:rad_number,
%!   +Value:or([atom,list(char),list(code),nonneg,string]),
%!   -Decimal:nonneg
%! ) is det.

to_decimal(dec, N, N):-
  nonneg(N), !.
to_decimal(Radix, Value, N):-
  radix_value(Radix, Base),
  to_chars(Value, Chars0),
  (   Chars0 = [-|Chars]
  ->  N #= -N0
  ;   Chars = Chars0,
      N #= N0
  ),
  positional_sum(Chars, Base, 0, N0).

positional_sum([], _, N, N):- !.
positional_sum([H|T], Base, M1, N):-
  radix_value(Base, H, M0),
  M2 is M1 * Base + M0,
  positional_sum(T, Base, M2, N).
