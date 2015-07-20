:- module(
  pair_ext,
  [
    'group_pairs_by_key@'/2, % +Pairs:list(pair)
                             % -GroupedPairs:list(pair)
    inverse_pair/2, % ?Pair:pair
                    % ?Inverse:pair
    json_pair/2, % ?Pair:pair
                 % ?Dict:dict
    list_pair/2, % ?List:list
                 % ?Pair:pair
    merge_pairs_by_key/2, % +Pairs:list(pair)
                          % -Merged:list(pair)
    number_of_equivalence_pairs/3, % +EquivalenceSets:list(ordset)
                                   % -NumberOfPairs:nonneg
                                   % +Options:list(nvpair)
    pair/3, % ?Pair:pair
            % ?Element1
            % ?Element2
    pair_element/2, % ?Pair:pair
                    % ?Element
    pair_first/2, % +Pair:pair
                  % ?First
    pair_list/2, % ?Pair:pair
                 % ?List:list
    pair_second/2, % +Pair:pair
                   % ?Second
    pairs_to_ascending_values/2, % +Pairs:list(pair)
                                 % -Values:list
    pairs_to_descending_values/2, % +Pairs:list(pair)
                                  % -Values:list
    pairs_to_set/2, % +Pairs:list(pair)
                    % -Members:list
    pairs_to_sets/2, % +Pairs:list(pair(iri))
                     % -Sets:list(ordset(iri))
    read_pairs_from_file/2, % +File:atom
                            % -Pairs:ordset(pair(atom))
    reflexive_pair/1, % ?Pair:pair
    remove_pairs/3, % +Original:ordset(pair)
                    % +Remove:ordset
                    % -Result:ordset(pair)
    set_to_pairs/3, % +Set:ordset
                    % :Comparator
                    % -Pairs:ordset(pair)
    sets_to_pairs/3, % +Sets:list(ordset)
                     % -Pairs:ordset(pair)
                     % +Options:list(nvpair)
    store_pairs_to_file/2, % +Pairs:list(pair(atom))
                           % +File:atom
    subpairs/3, % +Pairs:ordset(pair)
                % +Subkeys:ordset
                % -Subpairs:ordset(pair)
    term_to_pair/2 % @Term
                   % -Pair:pair
  ]
).

/** <module> Pair extensions

Support predicates for working with pairs.

@author Wouter Beek
@version 2013/09-2013/10, 2013/12, 2014/03, 2014/05, 2014/07-2014/12
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(lists), except([delete/3,subset/2])).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(plunit)).

:- use_module(plc(generics/list_ext)).

% Used for loading pairs from file.
:- dynamic(pair/2).

:- meta_predicate(set_to_pairs(+,2,-)).
:- meta_predicate(sets_to_pairs(+,2,+,-)).

:- op(555, xfx, ~).

:- predicate_options(number_of_equivalence_pairs/3, 3, [
     pass_to(cardinality_to_number_of_pairs/3, 3)
   ]).
:- predicate_options(cardinality_to_number_of_pairs/3, 3, [
     reflexive(+boolean)
   ]).
:- predicate_options(sets_to_pairs/3, 3, [
     reflexive(+boolean),
     symmetric(+boolean)
   ]).

:- multifile(error:has_type/2).
error:has_type(pair, _-_).
error:has_type(pair(Type), X-Y):-
  error:has_type(pair(Type,Type), X-Y).
error:has_type(pair(Type1,Type2), X-Y):-
  maplist(error:has_type, [Type1,Type2], [X,Y]).



% ! 'group_pairs_by_key@'(+Pairs:list(pair), -GroupedPairs:list(pair))
% is det.

'group_pairs_by_key@'([], []).
'group_pairs_by_key@'([M-N|T0], [M-[N|TN]|T]):-
  'same_key@'(M, T0, TN, T1),
  'group_pairs_by_key@'(T1, T).

'same_key@'(M0, [M-N|T0], [N|TN], T):-
  M0 =@= M, !,
  'same_key@'(M, T0, TN, T).
'same_key@'(_, L, [], L).



%! inverse_pair(+Pair:pair, -Inverse:pair) is det.
%! inverse_pair(-Pair:pair, +Inverse:pair) is det.

inverse_pair(X-Y, Y-X).



%! list_pair(+List:list, +Pair:pair) is semidet.
%! list_pair(+List:list, -Pair:pair) is det.
%! list_pair(-List:list, +Pair:pair) is det.

list_pair([X,Y], X-Y).



%! merge_pairs_by_key(+Pairs:list(pair), -Merged:list(pair)) is det.

merge_pairs_by_key([], []).
merge_pairs_by_key([K-V|T1], [K-VMerge|T3]):-
  same_key(K, [K-V|T1], VMerge, T2),
  merge_pairs_by_key(T2, T3).

same_key(K, [K-V|L1], VMerge2, L2):- !,
  same_key(K, L1, VMerge1, L2),
  ord_union(VMerge1, V, VMerge2).
same_key(K, [_|L1], VMerge, L2):- !,
  same_key(K, L1, VMerge, L2).
same_key(_, L, [], L).



%! number_of_equivalence_pairs(
%!   +EquivalenceSets:list(ordset),
%!   -NumberOfPairs:nonneg,
%!   +Options:list(nvpair)
%! ) is det.
% Returns the number of equivalence pairs that are encoded in
% the given collection of equivalence sets.
%
% The following options are supported:
%   * `reflexive(+boolean)`
%     Whether to count reflexive cases. Default: `true`.

number_of_equivalence_pairs(EqSets, NumberOfPairs, Options):-
  aggregate_all(
    sum(NumberOfPairs),
    (
      member(EqSet, EqSets),
      length(EqSet, Cardinality),
      cardinality_to_number_of_pairs(Cardinality, NumberOfPairs, Options)
    ),
    NumberOfPairs
  ).

cardinality_to_number_of_pairs(Cardinality, NumberOfPairs, Options):-
  NumberOfSymmetricAndTransitivePairs is Cardinality * (Cardinality - 1),
  (   option(reflexive(true), Options, true)
  ->  NumberOfPairs is NumberOfSymmetricAndTransitivePairs + Cardinality
  ;   NumberOfPairs = NumberOfSymmetricAndTransitivePairs
  ).



%! json_pair(+Pair:pair, +Dict:dict) is semidet.
%! json_pair(+Pair:pair, -Dict:dict) is det.
%! json_pair(-Pair:pair, +Dict:dict) is det.

json_pair(Pair, Dict):-
  dict_pairs(Dict, json, [Pair]).



%! pair(+Pair:pair, +X, +Y) is semidet.
%! pair(+Pair:pair, -X, -Y) is det.
%! pair(-Pair:pair, +X, +Y) is det.

pair(X-Y, X, Y).



%! pair_element(+Pair:pair, +Element) is semidet.
%! pair_element(+Pair:pair, -Element) is multi.

pair_element(X-_, X).
pair_element(_-Y, Y).



%! pair_first(+Pair:pair, +First) is semidet.
%! pair_first(+Pair:pair, -First) is det.

pair_first(X-_, X).



%! pair_list(+Pair:pair, +List:list) is semidet.
%! pair_list(+Pair:pair, -List:list) is det.
%! pair_list(-Pair:pair, +List:list) is det.

pair_list(X-Y, [X,Y]).



%! pair_second(+Pair:pair, +Second) is semidet.
%! pair_second(+Pair:pair, -Second) is det.

pair_second(X-_, X).



%! pairs_to_ascending_values(+Pairs:list(pair), -Values:list) is det.

pairs_to_ascending_values(Pairs1, Values):-
  keysort(Pairs1, Pairs2),
  pairs_values(Pairs2, Values).



%! pairs_to_descending_values(+Pairs:list(pair), -Values:list) is det.

pairs_to_descending_values(Pairs1, Values):-
  keysort(Pairs1, Pairs2),
  reverse(Pairs2, Pairs3),
  pairs_values(Pairs3, Values).



%! pairs_to_set(+Pairs:list(pair), -Set:ordset) is det.
% Returns the set of elements that occur in the given pairs.
%
% ### Example
%
% The following pairs:
% ```
% <a,b>
% <a,c>
% <d,e>
% ```
% result in the following set:
% ```
% {a,b,c,d,e}
% ```

pairs_to_set(Pairs, Members):-
  pairs_keys_values(Pairs, Keys1, Values1),
  maplist(sort, [Keys1,Values1], [Keys2,Values2]),
  ord_union(Keys2, Values2, Members).



%! pairs_to_sets(+Pairs:list(pair), -Sets:ordset(ordset)) is det.
% Returns the sets of elements that occur in the given pairs,
% when closed under transitivity.
%
% ### Example
%
% The following pairs:
% ```
% <a,b>
% <a,c>
% <d,e>
% ```
% result in the following sets:
% ```
% {{a,b,c},{d,e}}
% ```

pairs_to_sets(Pairs, Sets):-
  pairs_to_sets(Pairs, [], Sets).

pairs_to_sets([], AllSets, AllSets).
% Connect two sets.
pairs_to_sets([From-To|Pairs], Sets1, AllSets):-
  select(OldSet1, Sets1, Sets2),
  member(From, OldSet1),
  select(OldSet2, Sets2, Sets3),
  member(To, OldSet2), !,
  ord_union(OldSet1, OldSet2, NewSet),
  ord_add_element(Sets3, NewSet, Sets4),
  pairs_to_sets(Pairs, Sets4, AllSets).
% Add to an existing set.
pairs_to_sets([From-To|Pairs], Sets1, AllSets):-
  select(OldSet, Sets1, Sets2),
  (   member(From, OldSet)
  ->  ord_add_element(OldSet, To, NewSet)
  ;   member(To, OldSet)
  ->  ord_add_element(OldSet, From, NewSet)
  ), !,
  ord_add_element(Sets2, NewSet, Sets3),
  pairs_to_sets(Pairs, Sets3, AllSets).
% New set.
pairs_to_sets([From-To|Pairs], Sets1, AllSets):-
  list_to_ord_set([From,To], NewSet),
  ord_add_element(Sets1, NewSet, Sets2),
  pairs_to_sets(Pairs, Sets2, AllSets).



%! read_pairs_from_file(+File:atom, -Pairs:ordset(pair(atom))) is det.

read_pairs_from_file(File, Pairs):-
  setup_call_cleanup(
    ensure_loaded(File),
    aggregate_all(
      set(From-To),
      pair(From, To),
      Pairs
    ),
    unload_file(File)
  ).



%! reflexive_pair(?Pair:pair) is semidet.

reflexive_pair(X-X).



%! remove_pairs(
%!   +Original:ordset(pair),
%!   +RemoveKeys:ordset,
%!   -Result:ordset(pair)
%! ) is det.
% Assumes that the Original and Remove pairs are ordered in the same way.

remove_pairs(L1, RemoveKeys, L2):-
  pairs_keys(L1, Keys0),
  list_to_ord_set(Keys0, Keys),
  ord_subtract(Keys, RemoveKeys, RetainKeys),
  subpairs(L1, RetainKeys, L2).



%! set_to_pairs(
%!   +Set:ordset,
%!   :Comparator,
%!   -Pairs:ordset(pair)
%! ) is det.
% Returns the pairs that are represented by the given set.
%
% The following values are useful for the comparator:
%
% | *Comparator* | *Reflexive* | *Symmetic* |
% | `~`          | true        | true       |
% | `\=`         | false       | true       |
% | `@<`         | false       | false      |

set_to_pairs(Set, Comparator, Pairs):-
  aggregate_all(
    set(From-To),
    (
      member(From, To, Set),
      % No reflexive cases.
      call(Comparator, From, To)
    ),
    Pairs
  ).



%! sets_to_pairs(
%!   +Sets:list(ordset),
%!   -Pairs:ordset(pair),
%!   +Options:list(nvpair)
%! ) is det.
%
% The following options are supported:
%   * `reflexive(+boolean)`
%     Whether or not to return reflexive cases.
%     Default: `true`.
%   * `symmetric(+boolean)`
%     Whether or not to return symmetric pairs.
%     Default: `true`.

sets_to_pairs(Sets, Pairs, Options):-
  option(reflexive(Reflexive), Options),
  option(symmetric(Symmetric), Options),
  comparator(Reflexive, Symmetric, Comparator),
  sets_to_pairs(Sets, Comparator, [], Pairs).

sets_to_pairs([], _, AllPairs, AllPairs).
sets_to_pairs([Set|Sets], Comparator, Pairs1, AllPairs):-
  set_to_pairs(Set, Comparator, Pairs2),
  ord_union(Pairs1, Pairs2, Pairs3),
  sets_to_pairs(Sets, Pairs3, AllPairs).



%! store_pairs_to_file(+Pairs:list(pair(atom)), +File:atom) is det.

store_pairs_to_file(Pairs, File):-
  setup_call_cleanup(
    open(File, write, Stream),
    forall(
      member(From-To, Pairs),
      (
        writeq(Stream, pair(From,To)),
        write(Stream, '.'),
        nl(Stream)
      )
    ),
    close(Stream)
  ).



%! subpairs(
%!   +Pairs:ordset(pair),
%!   +Subkeys:ordset,
%!   -Subpairs:ordset(pair)
%! ) is det.
% This assumes that pairs and keys are ordered in the same way.

subpairs([], [], []).
subpairs([K-V|T1], [K|T2], [K-V|T3]):- !,
  subpairs(T1, T2, T3).
subpairs([_|T1], L2, L3):-
  subpairs(T1, L2, L3).



%! term_to_pair(@Term, -Pair:pair) is det.
% Retrusn the pair notation `First-Second` if the given term
% can be interpreted as a pair.
%
% The following pair notations are recognized:
%   1. `X-Y`
%   2. `X=Y`
%   3. `[X,Y]`
%   4. `X(Y)`

term_to_pair(X-Y, X-Y):- !.
term_to_pair(X=Y, X-Y):- !.
term_to_pair([X,Y], X-Y):- !.
term_to_pair(Compound, X-Y):-
  Compound =.. [X,Y].





% HELPERS

%! comparator(+Reflexive:boolean, +Symmetric:boolean, :Comparator) is det.

comparator(true,  true,  ~ ):- !.
comparator(false, true,  \=):- !.
comparator(false, false, @<):- !.





% UNIT TESTS

:- begin_tests(pair_ext).

% Base case.
pairs_to_ord_sets_example([], []).
% No multisets.
pairs_to_ord_sets_example([a-b,a-b], [[a,b]]).
% Reflexive case.
pairs_to_ord_sets_example([a-a], [[a]]).
% Symmetric case.
pairs_to_ord_sets_example([a-b,b-a], [[a,b]]).
% Separate sets.
pairs_to_ord_sets_example([a-b,c-d], [[a,b],[c,d]]).
% Merging sets.
pairs_to_ord_sets_example([a-b,c-d,d-b], [[a,b,c,d]]).

test(
  pairs_to_sets,
  [forall(pairs_to_ord_sets_example(Pairs,Sets)),true]
):-
  pairs_to_sets(Pairs, Sets).

:- end_tests(pair_ext).
