:- module(
  radix,
  [
    between_radix/3, % +Low:compound, +High:compound, ?Number:compound
    digits_radix/2 % ?Digits:list(hex), ?Number:compound
  ]
).

/** <module> Mathematics: Radix conversion

Predicate for transforming numbers between
positional notations of different radix.

@author Wouter Beek
@version 2018
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(clpfd)).
:- use_module(library(error)).

:- use_module(library/char_ext).
:- use_module(library/list_ext).
:- use_module(library/typecheck).
:- use_module(library/math_ext).

:- multifile
    error:has_type/2.

error:has_type(rad_name, Value):-
  error:has_type(oneof([bin,oct,dec,hex]), Value).
error:has_type(rad_number, Value):-
  error:has_type(oneof([2,8,10,16]), Value).





%! between_radix(+Low:compound, +High:compound, +Number:compound) is semidet.
%! between_radix(+Low:compound, +High:compound, -Number:compound) is nondet.

between_radix(Low, High, Number):-
  radconv(Low, dec(LowDec)),
  radconv(High, dec(HighDec)),
  % If Number is ground it is more efficient to first convert it to decimal
  % and then check whether it is between (rather than generating values
  % that are between).
  % This also makes the `(+,+,+)`-case semi-deterministic.
  (   ground(Number)
  ->  radconv(Number, dec(NumberDec)),
      between(LowDec, HighDec, NumberDec)
  ;   between(LowDec, HighDec, NumberDec),
      radconv(Number, dec(NumberDec))
  ).



%! digits_radix(+Digits:list(hex), -Number:compound) is det.
%! digits_radix(-Digits:list(hex), +Number:compound) is det.

digits_radix(Ds, N):-
  ground(Ds), !,
  maplist(digit_weight, Ds, Weights),
  integer_weights(N, Weights).
digits_radix(Ds, N):-
  ground(N), !,
  integer_weights(N, Weights),
  maplist(digit_weight, Ds, Weights).
digits_radix(_, _):-
  instantiation_error(_).
