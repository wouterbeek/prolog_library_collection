:- module(
  dict_ext,
  [
    dict_tag/3, % +Dict1:dict
                % +Tag:atom
                % ?Dict2:dict
    merge_dict/3, % +Dict1:dict
                  % +Dict2:dict
                  % -Dict:dict
    print_dict/1 % +Dict:dict
  ]
).

/** <module> Dictionary extensions

@author Wouter Beek
@version 2014/11, 2015/03-2015/04
*/

:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(dcg/dcg_pl_term)).





%! dict_tag(+Dict1:dict, +Tag:atom, +Dict2:dict) is semidet.
%! dict_tag(+Dict1:dict, +Tag:atom, -Dict2:dict) is det.
% Converts between dictionaries that differ only in their outer tag name.

dict_tag(Dict1, Tag, Dict2):-
  dict_pairs(Dict1, _, Pairs),
  dict_pairs(Dict2, Tag, Pairs).



%! merge_dict(+Dict1:dict, +Dict2:dict, -Dict:dict) is det.

merge_dict(D1, D2, D):-
  dict_pairs(D1, Tag1, P1),
  dict_pairs(D2, Tag2, P2),
  append(P1, P2, P),
  (Tag1 = Tag2 -> true ; Tag = Tag1),
  dict_pairs(D, Tag, P).



%! print_dict(Dict:dict) is det.

print_dict(Dict):-
  atom_phrase(dcg_pl_term(Dict), Atom),
  writeln(Atom).



Dict.toNumber(Key) := Number :-
  atom_number(Dict.Key, Number).

Dict1.subtract(Key,Value) := Dict2 :-
  get_dict(Key, Dict1, _, Dict2, Dict1.toNumber(Key) - Value).
