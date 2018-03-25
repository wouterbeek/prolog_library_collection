:- module(
  dict,
  [
    dict_delete_or_default/5, % +Key, +Dict1, +Default, -Value, -Dict2
    dict_get/3,               % ?Key, +Dict, -Value
    dict_get/4,               % +Key, +Dict, +Default, -Value
    dict_inc/2,               % +Key, +Dict
    dict_inc/3,               % +Key, +Dict, -Value
    dict_inc/4,               % +Key, +Dict, +Diff, -Value
    dict_key/2,               % +Dict, ?Key
    dict_pairs/2,             % ?Dict, ?Pairs
    dict_put/3,               % +Dict1, +Dict2, -Dict3
    dict_put/4,               % +Key, +Dict1, +Value, -Dict2
    dict_tag/2,               % +Dict, ?Tag
    dict_tag/3,               % +Dict1, ?Tag, -Dict2
    merge_dicts/2,            % +Dicts, -Dict
    merge_dicts/3             % +Dict1, +Dict2, -Dict3
  ]
).

/** <module> Dictionary extension

@author Wouter Beek
@version 2017-2018
*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).





%! dict_delete_or_default(+Key, +Dict1, +Default, -Value, -Dict2) is det.
%
% Either delete the Value for Key from Dict1 resulting in Dict2, or
% return the Default value and leave the dictionary unchanged.

dict_delete_or_default(Key, Dict1, _, Value, Dict2) :-
  del_dict(Key, Dict1, Value, Dict2), !.
dict_delete_or_default(_, Dict, Default, Default, Dict).



%! dict_get(?Key, +Dict, -Value) is nondet.

dict_get(Key, Dict, Value) :-
  get_dict(Key, Dict, Value).



%! dict_get(+Key, +Dict, +Default, -Value) is semidet.

dict_get(Key, Dict, _, Value) :-
  get_dict(Key, Dict, Value), !.
dict_get(_, _, Default, Default).



%! dict_inc(+Key, +Dict) is det.
%! dict_inc(+Key, +Dict, -Value) is det.
%! dict_inc(+Key, +Dict, +Diff, -Value) is det.

dict_inc(Key, Dict) :-
  dict_inc(Key, Dict, _).


dict_inc(Key, Dict, Value) :-
  dict_inc(Key, Dict, 1, Value).


dict_inc(Key, Dict, Diff, Value2) :-
  get_dict(Key, Dict, Value1),
  Value2 is Value1 + Diff,
  nb_set_dict(Key, Dict, Value2).



%! dict_key(+Dict, +Key) is semidet.
%! dict_key(+Dict, -Key) is nondet.

dict_key(Dict, Key) :-
  dict_get(Key, Dict, _).



%! dict_pairs(+Dict, +Pairs) is semidet.
%! dict_pairs(+Dict, -Pairs) is det.
%! dict_pairs(-Dict, +Pairs) is det.

dict_pairs(Dict, Pairs):-
  dict_pairs(Dict, _, Pairs).



%! dict_put(+Dict1, +Dict2, -Dict3) is det.

dict_put(Dict1, Dict2, Dict3) :-
  Dict3 = Dict1.put(Dict2).



%! dict_put(+Key, +DictIn, +Value, -DictOut) is det.

dict_put(Key, Dict1, Value, Dict2) :-
  put_dict(Key, Dict1, Value, Dict2).



%! dict_tag(+Dict, +Tag) is semidet.
%! dict_tag(+Dict, -Tag) is det.

dict_tag(Dict, Tag) :-
  dict_pairs(Dict, Tag, _).


%! dict_tag(+Dict1, +Tag, +Dict2) is semidet.
%! dict_tag(+Dict1, +Tag, -Dict2) is det.
%
% Converts between dictionaries that differ only in their outer tag name.

dict_tag(Dict1, Tag, Dict2):-
  dict_pairs(Dict1, _, Pairs),
  dict_pairs(Dict2, Tag, Pairs).



%! merge_dicts(+Dicts, -Dict) is det.
%
% A string of applications of merge_dicts/3, where newer dictionaries
% appear later in `Dicts'.

merge_dicts([], []).
merge_dicts([H], H) :- !.
merge_dicts([H1,H2|T1], T2) :-
  merge_dicts(H2, H1, H12),
  merge_dicts([H12|T1], T2).


%! merge_dicts(+NewDict, +OldDict, -Dict) is det.
%
% Merges two dictionaries into one new dictionary, similar to
% merge_options/3 from library(option).
%
% If NewDict and OldDict contain the same key then the value from
% NewDict is used, unless both are dicts, in which case the dicts are
% merged recursively.  If NewDict and OldDict have a different tag,
% then the tag from NewDict is used.

merge_dicts(NewDict, OldDict, Dict):-
  dict_pairs(OldDict, _, OldPairs0),
  dict_pairs(NewDict, Tag, NewPairs0),
  maplist(sort(1, @<), [OldPairs0,NewPairs0], [OldPairs,NewPairs]),
  merge_pairs_(OldPairs, NewPairs, Pairs),
  dict_pairs(Dict, Tag, Pairs).

% Key is only present in old dict.
merge_pairs_([Key1-Value1|T1], [Key2-Value2|T2], [Key1-Value1|T3]) :-
  Key1 @< Key2, !,
  merge_pairs_(T1, [Key2-Value2|T2], T3).
% Key is only present in old dict.
merge_pairs_([Key1-Value1|T1], [Key2-Value2|T2], [Key2-Value2|T3]) :-
  Key2 @< Key1, !,
  merge_pairs_([Key1-Value1|T1], T2, T3).
% Key is present in both dicts: either merge recursively (for dicts),
% or take the new value.
merge_pairs_([Key-Old|T1], [Key-New|T2], [Key-Value|T3]) :- !,
  (   maplist(is_dict, [Old,New])
  ->  merge_dicts(Old, New, Value)
  ;   Value = New
  ),
  merge_pairs_(T1, T2, T3).
merge_pairs_([], L, L) :- !.
merge_pairs_(L, [], L).
