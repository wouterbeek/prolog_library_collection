:- module(
  dict_ext,
  [
  % NEW
    dict_key/2,            % +Dict, ?Key
  % OLD
    atomize_dict/2,        % +Dict, -AtomizedDict
    create_dict/3,         % +Pairs, +Tag, -Dict
    create_grouped_sorted_dict/2, % +Pairs, -GroupedSortedDict
    create_grouped_sorted_dict/3, % +Pairs, +Tag, -GroupedSortedDict
    del_dict_or_default/5, % +Key, +Dict1, +Def, -Val, -Dict2
    dict_call_pairs/2,     % :Goal_1, +Dict
    dict_call_pairs/3,     % :Goal_2, +Dict1, -Dict2
    dict_create/2,         % -Dict, +Opts
    dict_dec/2,            % +Key, +Dict
    dict_dec/3,            % +Key, +Dict, -Val
    dict_dec/4,            % +Key, +Dict, +Diff, -Val
    dict_inc/2,            % +Key, +Dict
    dict_inc/3,            % +Key, +Dict, -Val
    dict_inc/4,            % +Key, +Dict, +Diff, -Val
    dict_pairs/2,          % ?Dict, ?Pairs
    dict_prepend/3,        % +Key, +Dict, +Elem
    dict_put/3,            % +Dict1, +Dict2, -Dict3
    dict_put_def/4,        % +Key, Dict1, +Def, +Dict2
    dict_put_pairs/3,      % +Dict1, +Pairs, -Dict2
    dict_remove_uninstantiated/2, % +Dict1, -Dict2
    dict_set/3,            % +Key, Dict, +Val
    dict_sum/2,            % +Dicts, -Dict
    dict_sum/3,            % +Dict1, +Dict2, -Dict3
    dict_tag/2,            % +Dict, -Tag
    dict_tag/3,            % +Dict1, +Tag, ?Dict2
    dicts_get/3,           % +Key, +Dicts, -Val
    dicts_getchk/3,        % +Key, +Dicts, -Val
    empty_dict/1,          % ?Dict
    get_dict/4,            % +Key, +Dict, +Def, -Val
    get_dict_path/3        % -Keys, +Dict, -Val
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





% NEW %

%! dict_key(+Dict, +Key) is semidet.
%! dict_key(+Dict, -Key) is nondet.

dict_key(Dict, Key) :-
  ground(Key), !,
  get_dict(Key, Dict, _).
dict_key(Dict, Key) :-
  dict_pairs(Dict, Pairs),
  member(Key-_, Pairs).





% OLD %

%! atomize_dict(+Dict, -AtomizedDict) is det.

atomize_dict(Dict1, Dict2):-
  atomize_dict0(Dict1, Dict2).

atomize_dict0(Dict1, Dict2):-
  is_dict(Dict1), !,
  dict_pairs(Dict1, Tag, Pairs1),
  maplist(atomize_dict0, Pairs1, Pairs2),
  dict_pairs(Dict2, Tag, Pairs2).
atomize_dict0(S, A):-
  string(S), !,
  atom_string(A, S).
atomize_dict0(X, X).



%! create_dict(+Pairs, +Tag, -Dict) is det.

create_dict(Pairs, Tag, Dict):-
  maplist(dict_pair, Pairs, Dicts),
  create_grouped_sorted_dict(Dicts, Tag, Dict).


dict_pair(Key1-Val1, Key2-Val2):-
  atom_string(Key2, Key1),
  (singleton_list(Val2, Val1), ! ; Val2 = Val1).



%! create_grouped_sorted_dict(+Pairs, -GroupedSortedDict) is det.
%! create_grouped_sorted_dict(+Pairs, ?Tag, -GroupedSortedDict) is det.

create_grouped_sorted_dict(Pairs, Dict):-
  create_grouped_sorted_dict(Pairs, _, Dict).


create_grouped_sorted_dict(Pairs, Tag, Dict):-
  sort(Pairs, SortedPairs),
  group_pairs_by_key(SortedPairs, GroupedPairs1),
  maplist(pair_flatten_singleton, GroupedPairs1, GroupedPairs2),
  dict_pairs(Dict, Tag, GroupedPairs2).



%! del_dict_or_default(+Key, +Dict1, +Default, -Value, -Dict2) is det.
%
% Either delete the Value for Key from Dict1 resulting in Dict2, or
% return the Default value and leave the dictionary intact.

del_dict_or_default(Key, Dict1, _, Val, Dict2) :-
  del_dict(Key, Dict1, Val, Dict2), !.
del_dict_or_default(_, Dict, Def, Def, Dict).



%! dict_call_pairs(:Goal_1, +Dict) is det.
%! dict_call_pairs(:Goal_2, +Dict1, -Dict2) is det.

dict_call_pairs(Goal_1, Dict) :-
  dict_pairs(Dict, Pairs),
  call(Goal_1, Pairs).


dict_call_pairs(Goal_2, Dict1, Dict2) :-
  dict_pairs(Dict1, Pairs1),
  maplist(Goal_2, Pairs1, Pairs2),
  dict_pairs(Dict2, Pairs2).



%! dict_create(-Dict, +Opts) is det.

dict_create(Dict, Opts) :-
  dict_create(Dict, _, Opts).



%! dict_dec(+Key, +Dict) is det.
%! dict_dec(+Key, +Dict, -Val) is det.
%! dict_dec(+Key, +Dict, +Diff, -Val) is det.

dict_dec(Key, Dict) :-
  dict_dec(Key, Dict, _).


dict_dec(Key, Dict, Val) :-
  dict_dec(Key, Dict, 1, Val).


dict_dec(Key, Dict, Diff, Val2) :-
  get_dict(Key, Dict, Val1),
  Val2 is Val1 - Diff,
  nb_set_dict(Key, Dict, Val2).



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



%! dict_pairs(+Dict, +Pairs) is semidet.
%! dict_pairs(+Dict, -Pairs) is det.
%! dict_pairs(-Dict, +Pairs) is det.

dict_pairs(Dict, Pairs):-
  dict_pairs(Dict, _, Pairs).



%! dict_put_def(+Key, +Dict1, +Def, -Dict2) is det.

dict_put_def(Key, Dict, _, Dict) :-
  dict_key(Dict, Key), !.
dict_put_def(Key, Dict1, Def, Dict2) :-
  Dict2 = Dict1.put(Key, Def).



%! dict_put_pairs(+Dict1, +Pairs, -Dict2) is det.

dict_put_pairs(Dict1, Pairs, Dict2) :-
  dict_pairs(Dict1, Pairs1),
  append(Pairs1, Pairs, Pairs2),
  dict_pairs(Dict2, Pairs2).



%! dict_prepend(+Key, +Dict, +Elem) is det.

dict_prepend(Key, Dict, H) :-
  get_dict(Key, Dict, T),
  nb_set_dict(Key, Dict, [H|T]).



%! dict_put(+Dict1, +Dict2, -Dict3) is det.

dict_put(Dict1, Dict2, Dict3) :-
  Dict3 = Dict1.put(Dict2).


%! dict_set(+Key, +Dict, +Val) is det.

dict_set(Key, Dict, Val) :-
  nb_set_dict(Key, Dict, Val).



%! dict_remove_uninstantiated(+Dict1, -Dict2) is det.

dict_remove_uninstantiated(Dict1, Dict2):-
  dict_pairs(Dict1, Tag, Pairs1),
  exclude(var_val, Pairs1, Pairs2),
  dict_pairs(Dict2, Tag, Pairs2).


var_val(_-Val):-
  var(Val).



%! dict_sum(+Dicts, -Dict) is det.
%! dict_sum(+Dict1, +Dict2, -Dict3) is det.

dict_sum(Dicts, Dict) :-
  dict_sum0(Dicts, _{}, Dict).

dict_sum0([], Dict, Dict) :- !.
dict_sum0([Dict1|T], Dict2, Dict4) :-
  dict_sum(Dict1, Dict2, Dict3),
  dict_sum0(T, Dict3, Dict4).


dict_sum(Dict1, Dict2, Dict3) :-
  maplist(dict_pairs, [Dict1,Dict2], [Pairs1,Pairs2]),
  pairs_sum(Pairs1, Pairs2, Pairs3),
  dict_pairs(Dict3, Pairs3).


pairs_sum([], Pairs, Pairs) :- !.
pairs_sum([Key-Val1|T1], Pairs2a, [Key-Val3|T3]) :-
  selectchk(Key-Val2, Pairs2a, Pairs2b), !,
  Val3 is Val1 + Val2,
  pairs_sum(T1, Pairs2b, T3).
pairs_sum([Key-Val|T1], Pairs2, [Key-Val|T3]) :-
  pairs_sum(T1, Pairs2, T3).



%! dict_tag(+Dict, -Tag) is det.

dict_tag(Dict, Tag) :-
  dict_pairs(Dict, Tag, _).


%! dict_tag(+Dict1, +Tag, +Dict2) is semidet.
%! dict_tag(+Dict1, +Tag, -Dict2) is det.
%
% Converts between dictionaries that differ only in their outer tag name.

dict_tag(Dict1, Tag, Dict2):-
  dict_pairs(Dict1, _, Ps),
  dict_pairs(Dict2, Tag, Ps).



%! dicts_get(+Key, +Dicts, -Val) is nondet.

dicts_get(Key, Dicts, Val) :-
  member(Dict, Dicts),
  get_dict(Key, Dict, Val).



%! dicts_getchk(+Key, +Dicts, -Val) is nondet.

dicts_getchk(Key, Dicts, Val) :-
  once(dicts_get(Key, Dicts, Val)).



%! empty_dict(@Term) is semidet.

empty_dict(_{}).



%! get_dict(+Key, +Dict, +Def, -Val) is semidet.

get_dict(Key, Dict, _, Val) :-
  get_dict(Key, Dict, Val), !.
get_dict(_, _, Def, Def).



%! get_dict_path(-Keys, +Dict, -Val) is nondet.

get_dict_path(L, Dict, Val) :-
  get_dict(H, Dict, Val0),
  (   is_dict(Val0)
  ->  get_dict_path(T, Val0, Val),
      L = [H|T]
  ;   Val = Val0,
      L = [H]
  ).
