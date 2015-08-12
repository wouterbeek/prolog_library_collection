Dict.toNumber(Key) := Number :-
  atom_number(Dict.Key, Number).

Dict1.subtract(Key,Value) := Dict2 :-
  get_dict(Key, Dict1, _, Dict2, Dict1.toNumber(Key) - Value).
