:- module(
  pair_ext,
  [
    group_pairs_by_key/3, % :Comparator_2
                          % +Pairs:list(pair)
                          % -GroupedPairs:list(pair)
    inverse_pair/2, % ?Pair:pair
                    % ?Inverse:pair
    is_reflexive_pair/1, % +Pair:pair)
    pair/3, % ?Pair:pair
            % ?Element1
            % ?Element2
    pair_element/2, % ?Pair:pair
                    % ?Element
    pair_key/2, % +Pair:pair
                % ?Key
    pair_has_var_key/1, % +Pair:pair
    pair_has_var_value/1, % +Pair:pair
    pair_list/2, % ?Pair:pair
                 % ?List:list
    pair_value/2, % +Pair:pair
                  % ?Value
    pairs_ordered_values/3, % +Pairs:list(pair)
                            % +Order:oneof([@<,@=<,@>,@>=])
                            % -Values:list
    pairs_to_set/2 % +Pairs:list(pair)
                   % -Set:ordset
  ]
).
:- reexport(library(pairs)).

/** <module> Pair extensions

Additional support for dealing with pairs.

@author Wouter Beek
@version 2015/08, 2015/10-2015/12
*/

:- use_module(library(apply)).
:- use_module(library(error)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

:- meta_predicate(group_pairs_by_key(2,+,-)).
:- meta_predicate(same_key(2,+,+,-,-)).

:- multifile(error:has_type/2).
error:has_type(pair, _-_).
error:has_type(pair(Type), X-Y):-
  error:has_type(pair(Type,Type), X-Y).
error:has_type(pair(Type1,Type2), X-Y):-
  maplist(error:has_type, [Type1,Type2], [X,Y]).





%! group_pairs_by_key(
%!   :Comparator_2,
%!   +Pairs:list(pair),
%!   -GroupedPairs:list(pair)
%! ) is det.

group_pairs_by_key(_, [], []):- !.
group_pairs_by_key(Comp, [M-N|T0], [M-[N|TN]|T]):-
  same_key(Comp, M, T0, TN, T1),
  group_pairs_by_key(Comp, T1, T).

same_key(Comp, M0, [M-N|T0], [N|TN], T):-
  call(Comp, M0, M), !,
  same_key(Comp, M, T0, TN, T).
same_key(_, _, L, [], L).



%! inverse_pair(+Pair:pair, -Inverse:pair) is det.
%! inverse_pair(-Pair:pair, +Inverse:pair) is det.

inverse_pair(X-Y, Y-X).



%! is_reflexive_pair(+Pair:pair) is semidet.

is_reflexive_pair(Pair):- pair(Pair, X, X).



%! pair(+Pair:pair, +X, +Y) is semidet.
%! pair(+Pair:pair, -X, -Y) is det.
%! pair(-Pair:pair, +X, +Y) is det.

pair(X-Y, X, Y).



%! pair_element(+Pair:pair, +Element) is semidet.
%! pair_element(+Pair:pair, -Element) is multi.
% Succeeds if Element occurs in Pair.

pair_element(X-_, X).
pair_element(X-Y, Y):- X \== Y.



%! pair_has_var_key(+Pair:pair) is semidet.

pair_has_var_key(K-_):- var(K).



%! pair_has_var_value(+Pair:pair) is semidet.

pair_has_var_value(_-V):- var(V).



%! pair_key(+Pair:pair, +First) is semidet.
%! pair_key(+Pair:pair, -First) is det.

pair_key(X-_, X).



%! pair_list(+Pair:pair, +List:list) is semidet.
%! pair_list(+Pair:pair, -List:list) is det.
%! pair_list(-Pair:pair, +List:list) is det.

pair_list(X-Y, [X,Y]).



%! pair_value(+Pair:pair, +Second) is semidet.
%! pair_value(+Pair:pair, -Second) is det.

pair_value(_-X, X).



%! pairs_ordered_values(+Pairs:list(pair), -Values:list) is det.
% | **Order** | **Ordering** | **Duplicate handling** |
% |:---------:|:------------:|:----------------------:|
% | `@<`      | ascending    | remove                 |
% | `@=<`     | ascending    | keep                   |
% | `@>`      | descending   | remove                 |
% | `@>=`     | descending   | keep                   |

pairs_ordered_values(Pairs, Order, Values):-
  sort(1, Order, Pairs, SortedPairs),
  pairs_values(SortedPairs, Values).



%! pairs_to_set(+Pairs:list(pair), -Set:ordset) is det.
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
