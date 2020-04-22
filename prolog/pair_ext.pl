:- module(
  pair_ext,
  [
    compound_pair/2, % ?Compound, ?Pair
    merge_pairs/3,   % +New, +Old, -Merge
    sum_value/2      % +Pair1, -Pair2
  ]
).
:- reexport(library(pairs)).

/** <module> Extended support for pairs

Extends the support for pairs in the SWI-Prolog standard library.

*/

:- use_module(library(error)).
:- use_module(library(lists)).

:- use_module(library(dict)).

:- multifile
    error:has_type/2.

error:has_type(pair(Type), Pair) :-
  error:has_type(pair(Type,Type), Pair).
error:has_type(pair(KeyType,ValueType), Key- Value) :-
  error:has_type(KeyType, Key),
  error:has_type(ValueType, Value).





%! compound_pair(+Compound:compound, +Pair:pair) is semidet.
%! compound_pair(+Compound:compound, -Pair:pair) is semidet.
%! compound_pair(-Compound:compound, +Pair:pair) is det.

compound_pair(Compound, Key-Value) :-
  compound_name_arguments(Compound, Key, [Value]).



%! merge_pairs(+New:list(pair), +Old:list(pair), -Merge:list(pair)) is det.

merge_pairs([], L, L) :- !.
merge_pairs(L, [], L) :- !.
% Key is only present in new dict: use it in merge.
merge_pairs([Key1-Value1|T1], [Key2-Value2|T2], [Key1-Value1|T3]) :-
  Key1 @< Key2, !,
  merge_pairs(T1, [Key2-Value2|T2], T3).
% Key is only present in old dict: use it in merge.
merge_pairs([Key1-Value1|T1], [Key2-Value2|T2], [Key2-Value2|T3]) :-
  Key2 @< Key1, !,
  merge_pairs([Key1-Value1|T1], T2, T3).
% Key is present in both dicts: either merge recursively (for dicts),
% or take the new value.
merge_pairs([Key-New|T1], [Key-Old|T2], [Key-Value|T3]) :-
  (   maplist(is_dict, [Old,New])
  ->  merge_dicts(New, Old, Value)
  ;   Value = New
  ),
  merge_pairs(T1, T2, T3).



%! sum_value(+Pair1:pair(term,number), -Pair2:pair(term,number)) is det.

sum_value(Key-Vals, Key-Val) :-
  sum_list(Vals, Val).
