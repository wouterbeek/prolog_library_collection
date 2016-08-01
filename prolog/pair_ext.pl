:- module(
  pair_ext,
  [
    asc_pairs/2,              % +Pairs, -AscendingPairs
    asc_pairs_values/2,       % +Pairs, -AscendingValues
    desc_pairs/2,             % +Pairs, -DescendingPairs
    desc_pairs_values/2,      % +Pairs, -DescendingValues
    group_pairs_by_key/3,     % :Comparator_2, +Pairs, -GroupedPairs
    is_pair/1,                % @Term
    is_reflexive_pair/1,      % +Pair
    pair/3,                   % ?Pair, ?Key, ?Value
    pair_edge/2,              % ?Pair, ?Edge
    pair_element/2,           % ?Pair, ?Element
    pair_flatten_singleton/2, % +Pair1, -Pair2
    pair_key/2,               % +Pair, ?Key
    pair_has_var_key/1,       % +Pair
    pair_has_var_value/1,     % +Pair
    pair_inv/2,               % ?Pair, ?InvPair
    pair_inv_list/2,          % ?Pair, ?InvL
    pair_inv_row/2,           % ?Pair, ?InvRow
    pair_list/2,              % ?Pair, ?L
    pair_row/2,               % ?Pair, ?Row
    pair_value/2,             % +Pair, ?Value
    pairs_to_set/2,           % +Pairs, -Elements
    sum_value/2               % +Pair, -Sum
  ]
).
:- reexport(library(pairs)).

/** <module> Pair extensions

Additional support for dealing with pairs.

@author Wouter Beek
@version 2015/08, 2015/10-2015/12, 2016/04, 2016/07
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

:- meta_predicate
    group_pairs_by_key(2, +, -),
    same_key(2, +, +, -, -).

:- multifile(error:has_type/2).
error:has_type(pair, _-_).
error:has_type(pair(Type), X-Y):-
  error:has_type(pair(Type,Type), X-Y).
error:has_type(pair(Type1,Type2), X-Y):-
  maplist(error:has_type, [Type1,Type2], [X,Y]).





%! asc_pairs(+Pairs, -AscendingPairs) is det.
% Sort Pairs in ascending order.

asc_pairs(L1, L2) :- keysort(L1, L2).



%! asc_pairs_values(+Pairs, -AscendingValues) is det.
% Sort the valus of Pairs in ascending order.

asc_pairs_values(L1, L3) :- asc_pairs(L1, L2), pairs_values(L2, L3).



%! desc_pairs(+Pairs, -AscendingPairs) is det.
% Sort Pairs in descending order.

desc_pairs(L1, L2) :- sort(1, @>=, L1, L2).



%! desc_pairs_values(+Pairs, -DescendingValues) is det.
% Sort the values of Pairs in descending order.

desc_pairs_values(L1, L3) :- desc_pairs(L1, L2), pairs_values(L2, L3).



%! group_pairs_by_key(:Comparator_2, +Pairs, -GroupedPairs) is det.

group_pairs_by_key(_, [], []):- !.
group_pairs_by_key(Comp, [M-N|T0], [M-[N|TN]|T]):-
  same_key(Comp, M, T0, TN, T1),
  group_pairs_by_key(Comp, T1, T).

same_key(Comp, M0, [M-N|T0], [N|TN], T):-
  call(Comp, M0, M), !,
  same_key(Comp, M, T0, TN, T).
same_key(_, _, L, [], L).



%! is_pair(@Term) is semidet.

is_pair(_-_).



%! is_reflexive_pair(+Pair) is semidet.

is_reflexive_pair(Pair):- pair(Pair, X, X).



%! pair(+Pair, +X, +Y) is semidet.
%! pair(+Pair, -X, -Y) is det.
%! pair(-Pair, +X, +Y) is det.

pair(X-Y, X, Y).



%! pair_edge(+Pair, -Edge) is det.
%! pair_edge(-Pair, +Edge) is det.

pair_edge(X-Y, edge(X,Y)).



%! pair_element(+Pair, +Element) is semidet.
%! pair_element(+Pair, -Element) is multi.
% Succeeds if Element occurs in Pair.

pair_element(X-_, X).
pair_element(X-Y, Y):- X \== Y.



%! pair_flatten_singleton(+Pair1, -Pair2) is det.

pair_flatten_singleton(Key-[Val], Key-Val) :- !.
pair_flatten_singleton(Key-Vals, Key-Vals).



%! pair_has_var_key(+Pair) is semidet.

pair_has_var_key(K-_):- var(K).



%! pair_has_var_value(+Pair) is semidet.

pair_has_var_value(_-V):- var(V).



%! pair_inv(+Pair, +InvPair) is semidet.
%! pair_inv(+Pair, -InvPair) is det.
%! pair_inv(-Pair, +InvPair) is det.

pair_inv(X-Y, Y-X).



%! pair_inv_list(+Pair, +InvL) is semidet.
%! pair_inv_list(+Pair, -InvL) is det.
%! pair_inv_list(-Pair, +InvL) is det.

pair_inv_list(X-Y, [Y,X]).



%! pair_inv_row(+Pair, +InvRow) is semidet.
%! pair_inv_row(+Pair, -InvRow) is det.
%! pair_inv_row(-Pair, +InvRow) is det.

pair_inv_row(X-Y, row(Y,X)).



%! pair_key(+Pair, +First) is semidet.
%! pair_key(+Pair, -First) is det.

pair_key(X-_, X).



%! pair_list(+Pair, +List) is semidet.
%! pair_list(+Pair, -List) is det.
%! pair_list(-Pair, +List) is det.

pair_list(X-Y, [X,Y]).



%! pair_row(+Pair, +Row) is semidet.
%! pair_row(+Pair, -Row) is det.
%! pair_row(-Pair, +Row) is det.

pair_row(X-Y, row(X,Y)).



%! pair_value(+Pair, +Second) is semidet.
%! pair_value(+Pair, -Second) is det.

pair_value(_-X, X).



%! pairs_to_set(+Pairs, -Set) is det.
% Returns the set of elements that occur in the given pairs.
%
% ### Example
%
% The following pairs:
%   * 〈a,b〉
%   * 〈a,c〉
%   * 〈d,e〉
% 
% result in the following set:
%   * {a,b,c,d,e}

pairs_to_set(Pairs, Set):-
  pairs_keys_values(Pairs, Keys, Vals),
  maplist(list_to_ord_set, [Keys,Vals], [SKeys,SVals]),
  ord_union(SKeys, SVals, Set).



%! sum_value(+Pair, -Value) is det.

sum_value(Key-Vals, Key-Val) :-
  sum_list(Vals, Val).
