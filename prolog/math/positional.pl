:- module(
  positional,
  [
    positional/2, % ?Number:nonneg
                  % ?Digits:list(between(0,9))
    positional/3, % ?Number:nonneg
                  % ?Base:nonneg
                  % ?Digits:list(between(0,9))
    positional_fraction/2 % ?Digits:list(between(0,9))
                          % ?Fraction:between(0.0,1.0)
  ]
).

/** <module> Positional notation

Support for positional number notation.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(aggregate)).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(math/math_ext)).





%% positional(+N:nonneg, +Digits:list(between(0,9))) is semidet.
%% positional(+N:nonneg, -Digits:list(between(0,9))) is det.
%% positional(-N:nonneg, +Digits:list(between(0,9))) is det.

positional(N, Ds):-
  positional(N, 10, Ds).

%! positional(+N:nonneg, +Base:nonneg, +Digits:list(between(0,9))) is semidet.
%! positional(+N:nonneg, +Base:nonneg, -Digits:list(between(0,9))) is multi.
%! positional(-N:nonneg, +Base:nonneg, +Digits:list(between(0,9))) is multi.
% @see http://stackoverflow.com/questions/4192063/reversible-binary-to-number-predicate/28442760#28442760
% @throws instantiation_error

positional(N, Base, Ds):-
  (   nonvar(N),
      nonvar(Base)
  ;   nonvar(Ds)
  ), !,
  positional(Ds, Base, 0, N, N).
positional(_, _, _):-
  instantiation_error(_).

positional([], _, N, N, _).
positional([D|Ds], Base, N0, N, M):-
  in_base(D, Base),
  N1 #= D + Base * N0,
  M #>= N1,
  positional(Ds, Base, N1, N, M).

in_base(X, Base):-
  succ(Max, Base),
  X in 0..Max.



%! positional_fraction(+Digits:list(between(0,9)), -Fraction:rational) is det.
%! positional_fraction(-Digits:list(between(0,9)), +Fraction:rational) is det.

positional_fraction(Ws, F):-
  ground(Ws),
  aggregate_all(
    sum(N),
    (
      nth1(I, Ws, W),
      N is W rdiv (10 ^ I)
    ),
    F
  ).
positional_fraction(Ws, F):-
  ground(F), !,
  fractional_integer(F, I),
  positional(I, Ws).
positional_fraction(_, _):-
  instantiation_error(_).





% TESTS %

:- begin_tests(positional).

test('positional(+,+,+)', [forall(positional_test(N, Base, L))]):-
  positional(N, Base, L).
test('positional(+,+,-)', [forall(positional_test(N, Base, L)),nondet]):-
  positional(N, Base, L0), L0 = L.
test('positional(-,+,+)', [forall(positional_test(N, Base, L)),nondet]):-
  positional(N0, Base, L), N0 = N.

positional_test(1226, 10, [1,2,2,6]).
positional_test(120, 60, [2,0]).

:- end_tests(positional).
