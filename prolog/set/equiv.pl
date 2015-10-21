:- module(
  equiv,
  [
    equiv_pairs_partition/2, % ?Pairs:list(pair)
                             % ?Partition:ordset(ordset)
    equiv_pairs_set/2 % ?Pairs:list(pair)
                      % ?Partition:ordset
  ]
).

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).

%! equiv(?Object, ?EquivalenceSet:ordset) is nondet.

:- thread_local(equiv/2).





%! equiv_pairs_partition(+Pairs:list(pair), -Partition:ordset(ordset)) is det.
%! equiv_pairs_partition(-Pairs:list(pair), +Partition:ordset(ordset)) is det.
% Converts between equivalence pairs and the corresponding domain partition.
%
% ### Example
%
% The pairs `{〈a,b〉,〈a,c〉,〈d,e〉}' correspond to
% the partition `{{a,b,c},{d,e}}'.

equiv_pairs_partition(L, Eqs):-
  ground(L), !,
  setup_call_cleanup(
    retractall(equiv(_,_)),
    equiv_pairs_partition0(L, Eqs),
    retractall(equiv(_,_))
  ).
equiv_pairs_partition(L, Eqs):-
  ground(Eqs), !,
  maplist(equiv_pairs_set, Ls, Eqs),
  append(Ls, L).
equiv_pairs_partition(L, Eqs):-
  instantiation_error(equiv_pairs_partition(L, Eqs)).

equiv_pairs_partition0([], Eq):- !,
  aggregate_all(set(Eq0), equiv(_, Eq0), Eq).
equiv_pairs_partition0([X-Y|T], Eq):-
  process_pair(X, Y),
  equiv_pairs_partition0(T, Eq).

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



%! equiv_pairs_set(+Pairs:list(pair), -EquivalenceSet:ordset) is det.
%! equiv_pairs_set(-Pairs:ordset(pair), +EquivalenceSet:ordset) is det.

equiv_pairs_set(L, Eqs):-
  ground(L), !,
  equiv_pairs_partition(L, [[Eqs]]).
equiv_pairs_set(L, Eqs):-
  ground(Eqs), !,
  aggregate_all(set(X-Y), member(X, Y, Eqs), L).
equiv_pairs_set(L, Eqs):-
  instantiation_error(equiv_pairs_set(L, Eqs)).





% UNIT TESTS %

:- begin_tests(equiv).

test(
  equiv_pairs_partition,
  [forall(equiv_pairs_partition_test(Pairs,Sets)),true]
):-
  equiv_pairs_partition(Pairs, Sets).

% Base case.
equiv_pairs_partition_test([], []).
% No multisets.
equiv_pairs_partition_test([a-b,a-b], [[a,b]]).
% Reflexive case.
equiv_pairs_partition_test([a-a], [[a]]).
% Symmetric case.
equiv_pairs_partition_test([a-b,b-a], [[a,b]]).
% Separate sets.
equiv_pairs_partition_test([a-b,c-d], [[a,b],[c,d]]).
% Merging sets.
equiv_pairs_partition_test([a-b,c-d,d-b], [[a,b,c,d]]).

:- end_tests(equiv).
