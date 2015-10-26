:- module(
  thread_ext,
  [
    print_thread/0,
    print_thread/1, % +Name:atom
    print_threads/0,
    thread_name/1 % -Name:atom
  ]
).

/** <module> Thread extensions

@author Wouter Beek
@license MIT license
@version 2015/10
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(dcg/dcg_phrase)).





print_thread:-
  thread_name(Name),
  print_thread(Name).


print_thread(Name):-
  dcg_with_output_to(user_output, thread(0, Name)).



print_threads:-
  % Print the threads in alphabetical order.
  aggregate_all(set(Name), thread_property(_, alias(Name)), Names),
  dcg_with_output_to(user_output, threads(0, Names)).



%! thread_name(-Name:atom) is det.
% Returns the name of the current thread.

thread_name(Name):-
  thread_self(Id),
  thread_property(Id, alias(Name)).





% GRAMMAR %

thread(I, Name) -->
  {
    thread_property(Id, alias(Name)),
    thread_property(Id, status(Status))
  },
  indent_nl(I, nvpair(atom(Name),atom(Status))).

thread_items(I, [H|T]) -->
  thread(I, H), !,
  thread_items(I, T).
thread_items(_, []) --> [].

threads(I1, L) -->
  {succ(I1, I2)},
  section(I1, "Thead overview", thread_items(I2, L)).
