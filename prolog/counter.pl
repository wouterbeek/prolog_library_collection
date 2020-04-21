:- module(
  counter,
  [
    counter_value/2,    % +Counter, ?N
    create_counter/1,   % -Counter
    increment_counter/1 % +Counter
  ]
).

/** <module> A simple counter

*/





%! counter_value(+Counter:compound, +N:nonneg) is semidet.
%! counter_value(+Counter:compound, -N:nonneg) is det.

counter_value(counter(N), N).



%! create_counter(-Counter:compound) is det.

create_counter(counter(0)).



%! increment_counter(+Counter:compound) is det.

increment_counter(Counter) :-
  arg(1, Counter, N1),
  N2 is N1 + 1,
  nb_setarg(1, Counter, N2).
