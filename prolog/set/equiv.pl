:- module(
  equiv,
  [
    equiv_pairs_to_sets/2 % +Pairs:list(pair)
                          % -Sets:ordset(ordset)
  ]
).

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(ordsets)).

:- thread_local(equiv/2).





%! equiv_pairs_to_sets(+Pairs:list(pair), -Sets:ordset(ordset)) is det.
% Returns the sets of elements that occur in the given pairs,
% when closed under transitivity.
%
% ### Example
%
% The following pairs:
%   * 〈a,b〉
%   * 〈a,c〉
%   * 〈d,e〉
% 
% result in the following sets:
%   * {a,b,c}
%   * {d,e}

equiv_pairs_to_sets(L, Eqs):-
  setup_call_cleanup(
    retractall(equiv(_,_)),
    equiv_pairs_to_sets0(L, Eqs),
    retractall(equiv(_,_))
  ).

equiv_pairs_to_sets0([], Eq):- !,
  aggregate_all(set(Eq0), equiv(_, Eq0), Eq).
equiv_pairs_to_sets0([X-Y|T], Eq):-
  process_pair(X, Y),
  equiv_pairs_to_sets0(T, Eq).

process_pair(X, Y):-
  equiv(X, EqX), !,
  (   equiv(Y, EqY)
  ->  % Both are present.
      merge_sets(EqX, EqY)
  ;   % Exactly one is present.
      add_to_set(Y, EqX)
  ).
% Exactly one is present (symmetric case).
process_pair(X, Y):-
  equiv(Y, EqY), !,
  add_to_set(X, EqY).
% Neither is present.
process_pair(X, Y):-
  list_to_ord_set([X,Y], S),
  assert(equiv(X, S)),
  assert(equiv(Y, S)).

add_to_set(Y, EqX):-
  ord_add_element(EqX, Y, EqXY),
  maplist(update_set(EqXY), EqX),
  assert(equiv(Y, EqXY)).

merge_sets(Eq, Eq):- !.
merge_sets(EqX, EqY):-
  ord_union(EqX, EqY, EqXY),
  maplist(update_set(EqXY), EqXY).

update_set(S, X):-
  retract(equiv(X, _)),
  assert(equiv(X, S)).
