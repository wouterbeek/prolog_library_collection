:- module(
  dict_ext,
  [
    atomize_dict/2,        % +D, -AtomizedD
    create_dict/3,         % +Pairs, +Tag, -D
    create_grouped_sorted_dict/2, % +Pairs, -GroupedSortedD
    create_grouped_sorted_dict/3, % +Pairs, +Tag, -GroupedSortedD
    del_dict_or_default/5, % +Key, +D1, +Def, -Val, -D2
    dict_call_pairs/2,     % :Goal_1, +D
    dict_call_pairs/3,     % :Goal_2, +D1, -D2
    dict_create/2,         % -Dict, +Opts
    dict_dec/2,            % +Key, +D
    dict_dec/3,            % +Key, +D, -Val
    dict_dec/4,            % +Key, +D, +Diff, -Val
    dict_get/3,            % +Key, +D, -Val
    dict_get/4,            % +Key, +D, +Def, -Val
    dict_has_key/2,        % +Key, +D
    dict_inc/2,            % +Key, +D
    dict_inc/3,            % +Key, +D, -Val
    dict_inc/4,            % +Key, +D, +Diff, -Val
    dict_pairs/2,          % ?D, ?Pairs
    dict_prepend/3,        % +Key, +D, +Elem
    dict_put/3,            % +D1, +D2, -D3
    dict_put_def/4,        % +Key, D1, +Def, +D2
    dict_put_pairs/3,      % +D1, +Pairs, -D2
    dict_remove_uninstantiated/2, % +D1, -D2
    dict_set/3,            % +Key, D, +Val
    dict_sum/2,            % +Dicts, -D
    dict_sum/3,            % +D1, +D2, -D3
    dict_tag/2,            % +Dict, -Tag
    dict_tag/3,            % +D1, +Tag, ?D2
    dicts_get/3,           % +Key, +Dicts, -Val
    dicts_getchk/3,        % +Key, +Dicts, -Val
    empty_dict/1,          % ?Dict
    get_dict_path/3,       % -Keys, +D, -Val
    merge_dicts/3          % +D1, +D2, -D3
  ]
).
:- reexport(library(dicts)).

/** <module> Dictionary extensions

@author Wouter Beek
@version 2015/08-2016/12
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(list_ext)).
:- use_module(library(pair_ext)).
:- use_module(library(yall)).

:- meta_predicate
    dict_call_pairs(1, +),
    dict_call_pairs(2, +, -).





%! atomize_dict(+D, -AtomizedD) is det.

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



%! create_dict(+Pairs, +Tag, -D) is det.

create_dict(Pairs, Tag, D):-
  maplist(dict_pair, Pairs, Dicts),
  create_grouped_sorted_dict(Dicts, Tag, D).


dict_pair(Key1-Val1, Key2-Val2):-
  atom_string(Key2, Key1),
  (singleton_list(Val2, Val1), ! ; Val2 = Val1).



%! create_grouped_sorted_dict(+Pairs, -GroupedSortedD) is det.
%! create_grouped_sorted_dict(+Pairs, ?Tag, -GroupedSortedD) is det.

create_grouped_sorted_dict(Pairs, D):-
  create_grouped_sorted_dict(Pairs, _, D).


create_grouped_sorted_dict(Pairs, Tag, D):-
  sort(Pairs, SortedPairs),
  group_pairs_by_key(SortedPairs, GroupedPairs1),
  maplist(pair_flatten_singleton, GroupedPairs1, GroupedPairs2),
  dict_pairs(D, Tag, GroupedPairs2).



%! del_dict_or_default(+Key, +Dict1, +Default, -Value, -Dict2) is det.
%
% Either delete the Value for Key from Dict1 resulting in Dict2, or
% return the Default value and leave the dictionary intact.

del_dict_or_default(Key, Dict1, _, Val, Dict2) :-
  del_dict(Key, Dict1, Val, Dict2), !.
del_dict_or_default(_, Dict, Def, Def, Dict).



%! dict_call_pairs(:Goal_1, +D) is det.
%! dict_call_pairs(:Goal_2, +D1, -D2) is det.

dict_call_pairs(Goal_1, D) :-
  dict_pairs(D, Pairs),
  call(Goal_1, Pairs).


dict_call_pairs(Goal_2, D1, D2) :-
  dict_pairs(D1, Pairs1),
  maplist(Goal_2, Pairs1, Pairs2),
  dict_pairs(D2, Pairs2).



%! dict_create(-Dict, +Opts) is det.

dict_create(Dict, Opts) :-
  dict_create(Dict, _, Opts).



%! dict_dec(+Key, +D) is det.
%! dict_dec(+Key, +D, -Val) is det.
%! dict_dec(+Key, +D, +Diff, -Val) is det.

dict_dec(Key, D) :-
  dict_dec(Key, D, _).


dict_dec(Key, D, Val) :-
  dict_dec(Key, D, 1, Val).


dict_dec(Key, D, Diff, Val2) :-
  get_dict(Key, D, Val1),
  Val2 is Val1 - Diff,
  nb_set_dict(Key, D, Val2).



%! dict_get(+Key, +D, -Val) is semidet.
%! dict_get(+Key, +D, +Def, -Val) is semidet.

dict_get(Key, D, Val) :-
  get_dict(Key, D, Val).


dict_get(Key, D, _, Val) :-
  dict_get(Key, D, Val), !.
dict_get(_, _, Def, Def).



%! dict_has_key(+Key, +D) is semidet.

dict_has_key(Key, D) :-
  get_dict(Key, D, _).



%! dict_inc(+Key, +Dict) is det.
%! dict_inc(+Key, +Dict, -Val) is det.
%! dict_inc(+Key, +Dict, +Diff, -Val) is det.

dict_inc(Key, Dict) :-
  dict_inc(Key, Dict, _).


dict_inc(Key, Dict, Val) :-
  dict_inc(Key, Dict, 1, Val).


dict_inc(Key, Dict, Diff, Val2) :-
  get_dict(Key, Dict, Val1),
  Val2 is Val1 + Diff,
  nb_set_dict(Key, Dict, Val2).



%! dict_pairs(+D, +Pairs) is semidet.
%! dict_pairs(+D, -Pairs) is det.
%! dict_pairs(-D, +Pairs) is det.

dict_pairs(D, L):-
  dict_pairs(D, _, L).



%! dict_put_def(+Key, +D1, +Def, -D2) is det.

dict_put_def(Key, D, _, D) :-
  dict_has_key(Key, D), !.
dict_put_def(Key, D1, Def, D2) :-
  D2 = D1.put(Key, Def).



%! dict_put_pairs(+D1, +Pairs, -D2) is det.

dict_put_pairs(D1, L, D2) :-
  dict_pairs(D1, L1),
  append(L1, L, L2),
  dict_pairs(D2, L2).



%! dict_prepend(+Key, +D, +Elem) is det.

dict_prepend(Key, D, H) :-
  get_dict(Key, D, T),
  nb_set_dict(Key, D, [H|T]).



%! dict_put(+D1, +D2, -D3) is det.

dict_put(D1, D2, D3) :-
  D3 = D1.put(D2).


%! dict_set(+Key, +D, +Val) is det.

dict_set(Key, D, Val) :-
  nb_set_dict(Key, D, Val).



%! dict_remove_uninstantiated(+D1, -D2) is det.

dict_remove_uninstantiated(D1, D2):-
  dict_pairs(D1, Tag, L1),
  exclude(var_val, L1, L2),
  dict_pairs(D2, Tag, L2).


var_val(_-Val):-
  var(Val).



%! dict_sum(+Dicts, -D) is det.
%! dict_sum(+D1, +D2, -D3) is det.

dict_sum(Dicts, D) :-
  dict_sum0(Dicts, _{}, D).

dict_sum0([], D, D) :- !.
dict_sum0([D1|T], D2, D4) :-
  dict_sum(D1, D2, D3),
  dict_sum0(T, D3, D4).


dict_sum(D1, D2, D3) :-
  maplist(dict_pairs, [D1,D2], [Pairs1,Pairs2]),
  pairs_sum(Pairs1, Pairs2, Pairs3),
  dict_pairs(D3, Pairs3).


pairs_sum([], Pairs, Pairs) :- !.
pairs_sum([Key-Val1|T1], L2a, [Key-Val3|T3]) :-
  selectchk(Key-Val2, L2a, L2b), !,
  Val3 is Val1 + Val2,
  pairs_sum(T1, L2b, T3).
pairs_sum([Key-Val|T1], L2, [Key-Val|T3]) :-
  pairs_sum(T1, L2, T3).



%! dict_tag(+Dict, -Tag) is det.

dict_tag(Dict, Tag) :-
  dict_pairs(Dict, Tag, _).


%! dict_tag(+D1, +Tag, +D2) is semidet.
%! dict_tag(+D1, +Tag, -D2) is det.
%
% Converts between dictionaries that differ only in their outer tag name.

dict_tag(D1, Tag, D2):-
  dict_pairs(D1, _, Ps),
  dict_pairs(D2, Tag, Ps).



%! dicts_get(+Key, +Dicts, -Val) is nondet.

dicts_get(Key, Dicts, Val) :-
  member(D, Dicts),
  get_dict(Key, D, Val).



%! dicts_getchk(+Key, +Dicts, -Val) is nondet.

dicts_getchk(Key, Dicts, Val) :-
  once(dicts_get(Key, Dicts, Val)).



%! empty_dict(@Term) is semidet.

empty_dict(_{}).



%! get_dict_path(-Keys, +D, -Val) is nondet.

get_dict_path(L, D, Val) :-
  get_dict(H, D, Val0),
  (   is_dict(Val0)
  ->  get_dict_path(T, Val0, Val),
      L = [H|T]
  ;   Val = Val0,
      L = [H]
  ).



%! merge_dicts(+D1, +D2, -D3) is det.
%
% Merges two dictionaries into one new dictionary.
%
% If D1 and D2 contain the same key then the value from D2 is used.
% If D1 and D2 do not have the same tag then the tag of D2 is used.

merge_dicts(D1, D2, D3):-
  dict_pairs(D1, Tag1, Ps1),
  dict_pairs(D2, Tag2, Ps2),
  dict_keys(D2, Keys2),
  exclude(key_in_keys0(Keys2), Ps1, OnlyPs1),
  append(OnlyPs1, Ps2, Ps3),
  (Tag1 = Tag2 -> true ; Tag3 = Tag2),
  dict_pairs(D3, Tag3, Ps3).

key_in_keys0(Keys, Key-_) :-
  memberchk(Key, Keys).
