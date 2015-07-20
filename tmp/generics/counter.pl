:- module(
  counter,
  [
    decrement_counter/1, % +Term
    enumerate_counter/2, % -Term
                         % -Count:integer
    get_count/2, % +Term
                 % ?Count:integer
    increment_counter/1, % +Term
    increment_counter/2 % +Term
                        % +Increment:integer
  ]
).

/** <module> Counter

@author Wouter Beek
@version 2014/12-2015/01
*/

:- use_module(library(error)).

%! counter0(?Hash:atom, ?Term:compound, ?Count:integer) is nondet.

:- dynamic(counter0/3).





%! decrement_counter(+Term) is det.
% @throws instantiation_error if Term is uninstantiated.

decrement_counter(Term):-
  increment_counter(Term, -1).



%! enumerate_counter(-Term, -Count:integer) is nondet.

enumerate_counter(Term, Count):-
  counter0(_, Term, Count).



%! get_count(+Term, +Count:integer) is semidet.
%! get_count(+Term, -Count:integer) is det.
% @throws instantiation_error if Term is uninstantiated.

get_count(Term, _):-
  var(Term), !,
  instantiation_error(Term).
get_count(Term, Count):-
  term_hash(Term, Hash),
  with_mutex(counter,
    counter0(Hash, Term, Count)
  ), !.
get_count(_, 0).



%! increment_counter(+Term) is det.
% @throws instantiation_error if Term is uninstantiated.

increment_counter(Term):-
  increment_counter(Term, 1).



%! increment_counter(+Term, +Increment:integer) is det.
% @throws instantiation_error if Term is uninstantiated.

increment_counter(Term, _):-
  var(Term), !,
  instantiation_error(Term).
increment_counter(Term, Increment):-
  with_mutex(counter, (
    retract(counter0(Hash, Term, Count0)),
    Count is Count0 + Increment,
    assert(counter0(Hash, Term, Count))
  )), !.
increment_counter(Term, Increment):-
  term_hash(Term, Hash),
  assert(counter0(Hash, Term, Increment)).

