:- module(
  thread_counter,
  [
    create_thread_counter/1,    % +Name
    delete_thread_counter/1,    % +Name
    delete_thread_counter/2,    % +Name, -Value
    delete_thread_counters/0,
    exists_thread_counter/1,    % +Name
    increment_thread_counter/1, % +Name
    increment_thread_counter/2, % +Name, -Count
    increment_thread_counter/3, % +Name, +Diff, -Count
    reset_thread_counter/1,     % +Name
    reset_thread_counter/2,     % +Name, -Count
    thread_counter/2            % ?Name, ?Count
  ]
).

/** <module> Thread-based counters

@author Wouter Beek
@version 2015/11, 2016/03
*/

:- use_module(library(error)).

%! thread_counter(?Name:compound, Count:integer) is nondet.

:- thread_local
   thread_counter/2.





%! create_thread_counter(+Name:compound) is det.
% Creates a counter with given name.
%
% @throws counter_exists If a counter with given name already exists.

create_thread_counter(Name):-
  (   thread_counter(Name, _)
  ->  throw(error(thread_counter_exists(Name), 'Thread-local counter already exists.'))
      %%%%delete_thread_counter(Name),%%%%HACK
      %%%%create_thread_counter(Name)%%%%HACK
  ;   assert(thread_counter(Name,0))
  ).



%! delete_thread_counter(+Name:compound) is nondet.
% Wrapper around delete_thread_counter/2.

delete_thread_counter(Name):-
  delete_thread_counter(Name, _).


%! delete_thread_counter(+Name:compound, -Integer) is nondet.
% Remove the counter(s) with the given name.
%
% @throws existence_error If no counter with the given name exists.

delete_thread_counter(Name, I):-
  thread_counter(Name, I), !,
  retractall(thread_counter(Name,_)).
delete_thread_counter(Name, _):-
  existence_error(thread_counter, Name).



delete_thread_counters:-
  retractall(thread_counter(_,_)).



exists_thread_counter(Name):-
  thread_counter(Name, _).



increment_thread_counter(Name):-
  increment_thread_counter(Name, 1, _).


increment_thread_counter(Name, Y):-
  increment_thread_counter(Name, 1, Y).


increment_thread_counter(Name, Diff, Y):-
  (   retract(thread_counter(Name,X))
  ->  Y is X + Diff
  ;   Y = 1
  ),
  assert(thread_counter(Name,Y)).



reset_thread_counter(Name):-
  reset_thread_counter(Name, _).


reset_thread_counter(Name, C):-
  (   retract(thread_counter(Name,C))
  ->  assert(thread_counter(Name,0))
  ;   existence_error(thread_counter, Name)
  ).
