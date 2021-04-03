:- module(
  counter,
  [
    counter_create/1,    % -Counter
    counter_increment/1, % +Counter
    counter_increment/2, % +Counter, -Value
    counter_value/2      % +Counter, ?Value
  ]
).

/** <module> A simple counter

*/

:- use_module(library(nb_ext)).



%! counter_create(-Counter:compound) is det.

counter_create(counter(0)).



%! counter_increment(+Counter:compound) is det.
%! counter_increment(+Counter:compound, -Value:nonneg) is det.

counter_increment(Counter) :-
  counter_increment(Counter, _).


counter_increment(Counter, Value) :-
  nb_increment(Counter, 1, Value).



%! counter_value(+Counter:compound, +N:nonneg) is semidet.
%! counter_value(+Counter:compound, -N:nonneg) is det.

counter_value(counter(N), N).
