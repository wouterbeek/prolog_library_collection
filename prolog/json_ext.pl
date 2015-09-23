:- module(
  json_ext,
  [
    clean_json/2 % +Json:dict
                 % -CleanJson:dict
  ]
).

/** <module> JSON extensions

@author Wouter Beek
@version 2015/09
*/

:- use_module(library(apply)).
:- use_module(library(dict_ext)).





%! clean_json(+Json:dict, -CleanJson:dict) is det.

clean_json(L1, L2):-
  is_list(L1), !,
  maplist(clean_json, L1, L2).
clean_json(D1, D2):-
  clean_dict(D1, D2).
