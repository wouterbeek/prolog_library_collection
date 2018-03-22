:- module(
  counter,
  [
    counter/2,           % ?Name, ?Count
    increment_counter/1, % +Name
    increment_counter/2, % +Name, -Count
    increment_counter/3  % +Name, +Diff, -Count
  ]
).

/** <module> Counter

@author Wouter Beek
@version 2018
*/

%! counter(?Name:compound, ?Count:number) is nondet.

:- dynamic
    counter/2.





%! increment_counter(+Name:compound) is det.
%! increment_counter(+Name:compound, -Count:number) is det.
%! increment_counter(+Name:compound, +Diff:number, -Count:number) is det.

increment_counter(Name) :-
  increment_counter(Name, _).


increment_counter(Name, N) :-
  increment_counter(Name, 1, N).


increment_counter(Name, Diff, N1) :-
  with_mutex(counter, (
    (   retract(counter(Name,N1))
    ->  N2 is N1 + Diff
    ;   N2 = 1
    ),
    assert(counter(Name,N2))
  )).
