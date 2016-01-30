:- module(
  dict_ext,
  [
    atomize_dict/2,               % +Dict, -AtomizedDict
    create_dict/3,                % +Pairs, +Tag, -Dict
    create_grouped_sorted_dict/2, % +Pairs, -GroupedSortedDict
    create_grouped_sorted_dict/3, % +Pairs, +Tag, -GroupedSortedDict
    dict_pairs/2,                 % ?Dict, ?Pairs
    dict_remove_uninstantiated/2, % +Dict1, -Dict2
    dict_tag/3,                   % +Dict1, +Tag, ?Dict2
    has_dict_key/2,               % +Dict, +Key
    is_empty_dict/1,              % @Term
    merge_dict/3,                 % +Dict1, +Dict2, -Dict
    print_dict/1,                 % +Dict
    print_dict/2                  % +Dict, +Indent
  ]
).
:- reexport(library(dicts)).

/** <module> Dictionary extensions

@author Wouter Beek
@version 2015/08-2015/11, 2016/01
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(list_ext)).
:- use_module(library(pairs)).
:- use_module(library(yall)).





%! atomize_dict(+Dict, -AtomizedDict) is det.

atomize_dict(D1, D2):-
  atomize_dict0(D1, D2).

atomize_dict0(D1, D2):-
  is_dict(D1), !,
  dict_pairs(D1, Tag, L1),
  maplist(atomize_dict0, L1, L2),
  dict_pairs(D2, Tag, L2).
atomize_dict0(S, A):-
  string(S), !,
  atom_string(A, S).
atomize_dict0(X, X).



%! create_dict(+Pairs, +Tag, -Dict) is det.

create_dict(Pairs, Tag, Dict):-
  maplist(dict_pair, Pairs, DictPairs),
  create_grouped_sorted_dict(DictPairs, Tag, Dict).

dict_pair(Key1-Val1, Key2-Val2):-
  atom_string(Key2, Key1),
  (singleton_list(Val2, Val1), ! ; Val2 = Val1).



%! create_grouped_sorted_dict(+Pairs, -GroupedSortedDict) is det.
%! create_grouped_sorted_dict(+Pairs, ?Tag, -GroupedSortedDict) is det.

create_grouped_sorted_dict(Pairs, D):-
  create_grouped_sorted_dict(Pairs, _, D).

create_grouped_sorted_dict(Pairs, Tag, D):-
  sort(Pairs, SortedPairs),
  group_pairs_by_key(SortedPairs, GroupedPairs),
  dict_pairs(D, Tag, GroupedPairs).



%! dict_pairs(+Dict, +Pairs) is semidet.
%! dict_pairs(+Dict, -Pairs) is det.
%! dict_pairs(-Dict, +Pairs) is det.

dict_pairs(D, L):-
  dict_pairs(D, _, L).



%! dict_remove_uninstantiated(+Dict1, -Dict2) is det.

dict_remove_uninstantiated(D1, D2):-
  dict_pairs(D1, Tag, L1),
  exclude(var_val, L1, L2),
  dict_pairs(D2, Tag, L2).
var_val(_-Val):- var(Val).



%! dict_tag(+Dict1, +Tag, +Dict2) is semidet.
%! dict_tag(+Dict1, +Tag, -Dict2) is det.
% Converts between dictionaries that differ only in their outer tag name.

dict_tag(Dict1, Tag, Dict2):-
  dict_pairs(Dict1, _, Ps),
  dict_pairs(Dict2, Tag, Ps).



%! has_dict_key(+Dict, +Key) is semidet.

has_dict_key(Dict, Key) :-
  get_dict(Key, Dict, _).



%! is_empty_dict(@Term) is semidet.

is_empty_dict(D):-
  is_dict(D),
  dict_pairs(D, _, L),
  empty_list(L).



%! merge_dict(+Dict1, +Dict2, -Dict) is det.
% Merges two dictionaries into one new dictionary.
% If Dict1 and Dict2 contain the same key then the value from Dict1 is used.
% If Dict1 and Dict2 do not have the same tag then the tag of Dict1 is used.

merge_dict(D1, D2, D):-
  dict_pairs(D1, Tag1, Ps1),
  dict_pairs(D2, Tag2, Ps2Dupl),
  pairs_keys(Ps1, Ks1),
  exclude([K]>>memberchk(K, Ks1), Ps2Dupl, Ps2),
  append(Ps1, Ps2, Ps),
  (Tag1 = Tag2 -> true ; Tag = Tag1),
  dict_pairs(D, Tag, Ps).



%! print_dict(+Dict) is det.
%! print_dict(+Dict, +Indent) is det.

print_dict(D):-
  print_dict(D, 0).

print_dict(D, I):-
  dcg_with_output_to(user_output, pl_term(D, I)),
  flush_output(user_output).
