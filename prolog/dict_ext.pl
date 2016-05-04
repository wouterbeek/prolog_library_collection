:- module(
  dict_ext,
  [
    atomize_dict/2,         % +Dict, -AtomizedDict
    create_dict/3,          % +Pairs, +Tag, -Dict
    create_grouped_sorted_dict/2, % +Pairs, -GroupedSortedDict
    create_grouped_sorted_dict/3, % +Pairs, +Tag, -GroupedSortedDict
    dict_has_key/2,         % +Key, +Dict
    dict_inc/2,             % +Key, +Dict
    dict_inc/3,             % +Key, +Dict, -Value
    dict_inc/4,             % +Key, +Dict, +Diff, -Value
    dict_pairs/2,           % ?Dict, ?Pairs
    dict_prepend/3,         % +Key, +Dict, +Elem
    dict_put_pairs/3,       % +Dict1, +Pairs, -Dict2
    dict_remove_uninstantiated/2, % +Dict1, -Dict2
    dict_tag/3,             % +Dict1, +Tag, ?Dict2
    get_dict/4,             % +Key, +Dict, -Value, +Default
    is_empty_dict/1,        % @Term
    merge_dict/3,           % +Dict1, +Dict2, -Dict3
    mod_dict/4,             % +Key, +Dict1,           -Value, -Dict2
    mod_dict/5              % +Key, +Dict1, +Default, -Value, -Dict2
  ]
).
:- reexport(library(dicts)).

/** <module> Dictionary extensions

@author Wouter Beek
@version 2015/08-2015/11, 2016/01, 2016/03-2016/04
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
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



%! dict_has_key(+Key, +Dict) is semidet.

dict_has_key(Key, Dict) :-
  catch(get_dict(Key, Dict, _), _, fail).



%! dict_inc(+Key, +Dict) is det.
%! dict_inc(+Key, +Dict, -Value) is det.
%! dict_inc(+Key, +Dict, +Diff, -Value) is det.

dict_inc(Key, Dict) :-
  dict_inc(Key, Dict, _).


dict_inc(Key, Dict, Val) :-
  dict_inc(Key, Dict, 1, Val).

dict_inc(Key, Dict, Diff, Val2) :-
  get_dict(Key, Dict, Val1),
  Val2 is Val1 + Diff,
  nb_set_dict(Key, Dict, Val2).



%! dict_pairs(+Dict, +Pairs) is semidet.
%! dict_pairs(+Dict, -Pairs) is det.
%! dict_pairs(-Dict, +Pairs) is det.

dict_pairs(D, L):-
  dict_pairs(D, _, L).



%! dict_put_pairs(+Dict1, +Pairs, -Dict2) is det.

dict_put_pairs(D1, L, D2) :-
  dict_pairs(D1, L1),
  append(L1, L, L2),
  dict_pairs(D2, L2).



%! dict_prepend(+Key, +Dict, +Elem) is det.

dict_prepend(Key, Dict, H) :-
  get_dict(Key, Dict, T),
  nb_set_dict(Key, Dict, [H|T]).



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



%! get_dict(+Key, +Dict, -Value, +Default) is det.

get_dict(K, D, V, _) :-
  dict_has_key(K, D), !,
  get_dict(K, D, V).
get_dict(_, _, Def, Def).



%! is_empty_dict(@Term) is semidet.

is_empty_dict(D):-
  is_dict(D),
  dict_pairs(D, _, L),
  empty_list(L).



%! merge_dict(+Dict1, +Dict2, -Dict3) is det.
% Merges two dictionaries into one new dictionary.
% If Dict1 and Dict2 contain the same key then the value from Dict2 is used.
% If Dict1 and Dict2 do not have the same tag then the tag of Dict2 is used.

merge_dict(D1, D2, D3):-
  dict_pairs(D1, Tag1, Ps1),
  dict_pairs(D2, Tag2, Ps2),
  dict_keys(D2, Keys2),
  exclude(key_in_keys0(Keys2), Ps1, OnlyPs1),
  append(OnlyPs1, Ps2, Ps3),
  (Tag1 = Tag2 -> true ; Tag3 = Tag2),
  dict_pairs(D3, Tag3, Ps3).

key_in_keys0(Keys, Key-_) :- memberchk(Key, Keys).



%! mod_dict(+Key, +Dict1, -Value, -Dict2) is det.

mod_dict(Key, Dict1, Val, Dict2) :-
  dict_has_key(Key, Dict1),
  del_dict(Key, Dict1, Val, Dict2).


%! mod_dict(+Key, +Dict1, +Default, -Value, -Dict2) is det.

mod_dict(Key, Dict1, _, Val, Dict2) :-
  mod_dict(Key, Dict1, Val, Dict2), !.
mod_dict(_, Dict, Def, Def, Dict).
