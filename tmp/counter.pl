:- module(
  counter,
  [
    create_counter/1,    % +Name
    delete_counter/1,    % +Name
    delete_counter/2,    % +Name, -Count
    exists_counter/1,    % +Name
    get_counter/2,       % +Name, -Count
    increment_counter/1, % +Name
    increment_counter/2, % +Name, -Count
    increment_counter/3, % +Name, +Diff, -Count
    reset_counter/1,     % +Name
    reset_counter/2      % +Name, -Count
  ]
).

/** <module> Counters

@author Wouter Beek
@version 2015
*/

:- use_module(library(error)).

%! counter(?Name:compound, Count:integer) is nondet.

:- dynamic
    counter/2.





%! create_counter(+Name:compound) is det.
%
% Creates a counter with the given name.
%
% @throws existence_error If a counter with given name already exists.

create_counter(Name) :-
  with_mutex(counter, (
    (   counter(Name, _)
    ->  existence_error(counter, Name)
    ;   assert(counter(Name, 0))
    )
  )).



%! delete_counter(+Name:compound) is det.
%! delete_counter(+Name:compound, -Value:integer) is det.
%
% Removes the counter with the given name and returns its value.
%
% @throws existence_error If no counter with given name exists.

delete_counter(Name) :-
  delete_counter(Name, _).



delete_counter(Name, N) :-
  with_mutex(counter, (
    (   retract(counter(Name, N))
    ->  true
    ;   existence_error(counter, Name)
    )
  )).



exists_counter(Name) :-
  counter(Name, _).



get_counter(Name, C) :-
  counter(Name, C).



reset_counter(Name) :-
  reset_counter(Name, _).

reset_counter(Name, C) :-
  with_mutex(counter, (
    (   retract(counter(Name,C))
    ->  assert(counter(Name,0))
    ;   existence_error(counter, Name)
    )
  )).
