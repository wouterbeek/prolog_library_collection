:- module(
  json_ext,
  [
    atomize_json/2 % +Json:dict
                   % -AtomizedJson:dict
  ]
).

/** <module> JSON extensions

@author Wouter Beek
@version 2015/09-2015/10
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).





%! atomize_json(+Json:dict, -AtomizedJson:dict) is det.

atomize_json(L1, L2):-
  is_list(L1), !,
  maplist(atomize_json, L1, L2).
atomize_json(D1, D2):-
  atomize_dict(D1, D2).
