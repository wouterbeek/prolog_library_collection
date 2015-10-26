:- module(
  count_ext,
  [
    create_counter/1, % +Name:compound
    create_thread_counter/2, % +Suggestion:atom
                             % -Name:compound
    delete_counter/1, % +Name:compound
    delete_counter/2, % +Name:compound
                      % -Count:integer
    exists_counter/1, % +Name:compound
    get_counter/2, % +Name:compound
                   % -Count:integer
    increment_counter/1, % +Name:compound
    increment_counter/2, % +Name:compound
                         % -Count:integer
    increment_counter/3, % +Name:compound
                         % +Diff:integer
                         % -Count:integer
    reset_counter/1, % +Name:compound
    reset_counter/2 % +Name:compound
                    % -Count:integer
  ]
).

/** <module> Counter extension

@author Wouter Beek
@license MIT license
i@version 2015/09-2015/10
*/

:- use_module(library(error)).
:- use_module(library(os/thread_ext)).

%! counter(?Name:compound, Count:integer) is nondet.

:- dynamic(counter/2).





create_counter(N):-
  with_mutex(count_ext, (
    (   counter(N, _)
    ->  throw(error(counter_exists(N), 'Counter already exists.'))
    ;   assert(counter(N,0))
    )
  )).



create_thread_counter(N1, N):-
  thread_name(N2),
  N =.. [N1,N2],
  create_counter(N).



delete_counter(N):-
  delete_counter(N, _).


delete_counter(N, C):-
  with_mutex(count_ext, (
    (retract(counter(N,C)) -> true ; existence_error(counter, N))
  )).



exists_counter(N):-
  counter(N, _).



get_counter(N, C):-
  counter(N, C).



increment_counter(N):-
  increment_counter(N, 1, _).


increment_counter(N, X):-
  increment_counter(N, 1, X).

increment_counter(N, Diff, X):-
  with_mutex(count_ext, (
    (   retract(counter(N,X))
    ->  Y is X + Diff
    ;   Y = 1
    ),
    assert(counter(N,Y))
  )).



reset_counter(N):-
  reset_counter(N, _).

reset_counter(N, C):-
  with_mutex(count_ext, (
    (   retract(counter(N,C))
    ->  assert(counter(N,0))
    ;   existence_error(counter, N)
    )
  )).
