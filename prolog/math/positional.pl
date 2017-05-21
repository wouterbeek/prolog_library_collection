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
