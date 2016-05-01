:- module(
  pair_ext,
  [
    asc_pairs/2,           % +Pairs, -AscendingPairs
    asc_pairs_values/2,    % +Pairs, -AscendingValues
    desc_pairs/2,          % +Pairs, -DescendingPairs
    desc_pairs_values/2,   % +Pairs, -DescendingValues
    group_pairs_by_key/3,  % :Comparator_2, +Pairs, -GroupedPairs
    inverse_pair/2,        % ?Pair, ?InversePair
    is_pair/1,             % @Term
    is_reflexive_pair/1,   % +Pair
    pair/3,                % ?Pair, ?Key, ?Value
    pair_element/2,        % ?Pair, ?Element
    pair_key/2,            % +Pair, ?Key
    pair_has_var_key/1,    % +Pair
    pair_has_var_value/1,  % +Pair
    pair_list/2,           % ?Pair, ?List
    pair_row/2,            % ?Pair, ?Row
    pair_value/2,          % +Pair, ?Value
    pairs_to_set/2,        % +Pairs, -Elements
    sum_value/2            % +Pair, -Sum
  ]
).
:- reexport(library(pairs)).

/** <module> Pair extensions

Additional support for dealing with pairs.

@author Wouter Beek
@version 2015/08, 2015/10-2015/12, 2016/04
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



%! inverse_pair(+Pair, -Inverse) is det.
%! inverse_pair(-Pair, +Inverse) is det.

inverse_pair(X-Y, Y-X).



%! is_pair(@Term) is semidet.

is_pair(_-_).



%! is_reflexive_pair(+Pair) is semidet.

is_reflexive_pair(Pair):- pair(Pair, X, X).



%! pair(+Pair, +X, +Y) is semidet.
%! pair(+Pair, -X, -Y) is det.
%! pair(-Pair, +X, +Y) is det.

pair(X-Y, X, Y).



%! pair_element(+Pair, +Element) is semidet.
%! pair_element(+Pair, -Element) is multi.
% Succeeds if Element occurs in Pair.

pair_element(X-_, X).
pair_element(X-Y, Y):- X \== Y.



%! pair_has_var_key(+Pair) is semidet.

pair_has_var_key(K-_):- var(K).



%! pair_has_var_value(+Pair) is semidet.

pair_has_var_value(_-V):- var(V).



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
