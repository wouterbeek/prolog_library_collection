:- module(
  thread_counter,
  [
    create_thread_counter/1, % +Name:compound
    delete_thread_counter/1, % +Name
    delete_thread_counter/2, % +Name:compound
                             % -Count:integer
    exists_thread_counter/1, % +Name:compound
    increment_thread_counter/1, % +Name
    increment_thread_counter/2, % +Name, -Count
    increment_thread_counter/3, % +Name:compound
                                % +Diff:integer
                                % -Count:integer
    reset_thread_counter/1, % +Name
    reset_thread_counter/2, % +Name:compound
                            % -Count:integer
    thread_counter/2 % ?Name:compound
                     % ?Count:integer
  ]
).

/** <module> Thread-based counters

@author Wouter Beek
@license MIT license
@version 2015/11
*/

:- use_module(library(error)).

%! thread_counter(?Name:compound, Count:integer) is nondet.

:- thread_local(thread_counter/2).





%! create_thread_counter(+Name:compound) is det.
% Creates a counter with given name.
%
% @throws counter_exists If a counter with given name already exists.

create_thread_counter(N):-
  (   thread_counter(N, _)
  ->  throw(error(thread_counter_exists(N), 'Thread-local counter already exists.'))
  ;   assert(thread_counter(N,0))
  ).



%! delete_thread_counter(+Name:compound) is det.
% Wrapper around delete_thread_counter/2 that does not return
% the value of the counter.

delete_thread_counter(N):-
  delete_thread_counter(N, _).


%! delete_thread_counter(+Name:compound, -Value:integer) is det.
% Removed the counter with the given name and returns its value.
%
% @throws existence_error If no counter with given name exists.

delete_thread_counter(N, C):-
  (retract(thread_counter(N,C)) -> true ; existence_error(thread_counter, N)).



exists_thread_counter(N):-
  thread_counter(N, _).



increment_thread_counter(N):-
  increment_thread_counter(N, 1, _).


increment_thread_counter(N, X):-
  increment_thread_counter(N, 1, X).


increment_thread_counter(N, Diff, X):-
  (   retract(thread_counter(N,X))
  ->  Y is X + Diff
  ;   Y = 1
  ),
  assert(thread_counter(N,Y)).



reset_thread_counter(N):-
  reset_thread_counter(N, _).


reset_thread_counter(N, C):-
  (   retract(thread_counter(N,C))
  ->  assert(thread_counter(N,0))
  ;   existence_error(thread_counter, N)
  ).
