:- module(
  positional,
  [
    clpfd_positional/2, % ?Digits:list(between(0,9))
                        % ?Integer:nonneg
    clpfd_positional/3, % ?Digits:list(between(0,9))
                        % ?Base:nonneg
                        % ?Integer:nonneg
    positional/2, % ?Digits:list(between(0,9))
                  % ?Number:nonneg
    positional/3, % ?Digits:list(between(0,9))
                  % ?Base:nonneg
                  % ?Number:nonneg
    positional_fraction/2 % ?Digits:list(between(0,9))
                          % ?Fraction:between(0.0,1.0)
  ]
).

/** <module> Positional notation

Support for positional number notation.

@author Wouter Beek
@version 2015/07, 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(clpfd)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(math/math_ext)).

:- multifile(clpfd:run_propagator/2).





%! clpfd_positional(+Digits:list(between(0,9)), +Integer:nonneg) is semidet.
%! clpfd_positional(+Digits:list(between(0,9)), -Integer:nonneg) is det.
%! clpfd_positional(-Digits:list(between(0,9)), +Integer:nonneg) is det.
% ### Example
%
% ```prolog
% year(Y) -->
%   {clpfd_positional([Y1,Y2,Y3,Y4], Y)},
%   #(4, 'DIGIT', [Y1,Y2,Y3,Y4], []).
% ```

clpfd_positional(Ds, N):-
  clpfd_positional(Ds, 10, N).


%! clpfd_positional(+Digits:list(between(0,9)), +Base:nonneg, +Integer:nonneg) is semidet.
%! clpfd_positional(+Digits:list(between(0,9)), +Base:nonneg, -Integer:nonneg) is det.
%! clpfd_positional(-Digits:list(between(0,9)), +Base:nonneg, +Integer:nonneg) is det.

clpfd_positional(Ds, Base, N):-
  clpfd:make_propagator(clpfd_positional(Ds, Base, N), Prop),
  clpfd:init_propagator(N, Prop),
  clpfd:init_propagator(Base, Prop),
  maplist(flip_init_propagator(Prop), Ds),
  clpfd:trigger_once(Prop).

clpfd:run_propagator(clpfd_positional(Ds, Base, N), MState):-
  (   (   maplist(error:has_type(nonneg), [N,Base])
      ;   maplist(error:has_type(between(0, 9)), Ds)
      )
  ->  clpfd:kill(MState),
      positional(Ds, Base, N)
  ;   \+ ground([N|Ds])
  ).

flip_init_propagator(Prop, Arg):-
  clpfd:init_propagator(Arg, Prop).



%! positional(+Digits:list(between(0,9)), +N:nonneg) is semidet.
%! positional(+Digits:list(between(0,9)), -N:nonneg) .
%! positional(-Digits:list(between(0,9)), +N:nonneg) .

positional(Ds, N):-
  positional(Ds, 10, N).


%! positional(+Digits:list(between(0,9)), +Base:nonneg, +N:nonneg) is semidet.
%! positional(+Digits:list(between(0,9)), +Base:nonneg, -N:nonneg) .
%! positional(-Digits:list(between(0,9)), +Base:nonneg, +N:nonneg) .
% @see http://stackoverflow.com/questions/4192063/reversible-binary-to-number-predicate/28442760#28442760
% @throws instantiation_error

positional(Ds, Base, N):-
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


:- begin_tests('positional/3').

test(
  'positional(+,+,+) is semidet. TRUE',
  [forall(positional_test(L,Base,N))]
):-
  positional(L, Base, N).
test(
  'positional(+,+,-) is multi. TRUE',
  [forall(positional_test(L,Base,N)),nondet]
):-
  positional(L0, Base, N), L0 = L.
test(
  'positional(-,+,+) is multi. TRUE',
  [forall(positional_test(L, Base, N)),nondet]
):-
  positional(L, Base, N0), N0 = N.

positional_test([1,2,2,6], 10, 1226).
positional_test([2,0], 60, 120).

:- end_tests('positional/3').



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
