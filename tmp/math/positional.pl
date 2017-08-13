:- module(
  positional,
  [
    clpfd_positional/2, % ?Digits, ?Integer
    clpfd_positional/3  % ?Digits, ?Base, ?Integer
  ]
).

/** <module> Positional notation

Support for positional number notation.

@author Wouter Beek
@version 2015/07, 2015/11
*/

:- use_module(library(aggregate)).
:- use_module(library(error)).
:- use_module(library(lists)).
:- use_module(library(math_ext)).

:- multifile(clpfd:run_propagator/2).





%! clpfd_positional(+Digits:list(between(0,9)), +Integer:nonneg) is semidet.
%! clpfd_positional(+Digits:list(between(0,9)), -Integer:nonneg) is det.
%! clpfd_positional(-Digits:list(between(0,9)), +Integer:nonneg) is det.
%
% # Example
%
% ```prolog
% year(Y) -->
%   {clpfd_positional([Y1,Y2,Y3,Y4], Y)},
%   #(4, 'DIGIT', [Y1,Y2,Y3,Y4], []).
% ```

clpfd_positional(Weights, N):-
  clpfd_positional(Weights, 10, N).


%! clpfd_positional(+Digits:list(between(0,9)), +Base:nonneg, +Integer:nonneg) is semidet.
%! clpfd_positional(+Digits:list(between(0,9)), +Base:nonneg, -Integer:nonneg) is det.
%! clpfd_positional(-Digits:list(between(0,9)), +Base:nonneg, +Integer:nonneg) is det.

clpfd_positional(Weights, Base, N):-
  clpfd:make_propagator(clpfd_positional(Weights, Base, N), Prop),
  clpfd:init_propagator(N, Prop),
  clpfd:init_propagator(Base, Prop),
  maplist(flip_init_propagator(Prop), Weights),
  clpfd:trigger_once(Prop).

clpfd:run_propagator(clpfd_positional(Weights, Base, N), MState):-
  (   (   maplist(error:has_type(nonneg), [N,Base])
      ;   maplist(error:has_type(between(0, 9)), Weights)
      )
  ->  clpfd:kill(MState),
      integer_fractional(N, Base, Weights)
  ;   \+ ground([N|Weights])
  ).

flip_init_propagator(Prop, Arg):-
  clpfd:init_propagator(Arg, Prop).
