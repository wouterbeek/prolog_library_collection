:- module(
  type_checking,
  [
    type_check/2, % +Type:compound
                  % +Value:atom
    uri/1 % ?Resource
  ]
).

/** <module> Type checking

Predicates used for parsing and checking value-type conformance.

@author Wouter Beek
@version 2013/01
*/



%% type_check(+Type:compound, +Value:term) is semidet.
% Succeeds if the given value is of the given type.
%
% @param Type A compound term representing a type.
% @param Value The atomic name of a value.

type_check(or(AlternativeTypes), Value):-
  member(Type, AlternativeTypes),
  type_check(Type, Value),
  !.
type_check(boolean, Value):-
  type_check(oneof([false,true]), Value).
type_check(double, Value):-
  type_check(float, Value),
  !.
type_check(oneof(Values), Value):-
  memberchk(Value, Values),
  !.
type_check(Type, Value):-
  must_be(Type, Value),
  !.
% DCG defined types
type_check(Type, Value):-
  atom_chars(Value, ValueChars),
  Call =.. [Type, ValueChars, []],
  call(Call).

%% uri(?Resource:uri) is semidet.

uri(Resource):-
  uri_components(
    Resource,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  nonvar(Scheme),
  nonvar(Authority),
  nonvar(Path).

