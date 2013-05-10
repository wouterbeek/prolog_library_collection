:- module(
  typecheck,
  [
    typecheck/2, % +Type:compound
    is_uri/1
  ]
).

/** <module> Type checking

Predicates used for parsing and checking value-type conformance.

@author Wouter Beek
@version 2013/01
*/



%% typecheck(+Type:compound, +Value) is semidet.
% Succeeds if the given value is of the given type.
%
% @param Type A compound term representing a type.
% @param Value

typecheck(or(AlternativeTypes), Value):-
  member(Type, AlternativeTypes),
  typecheck(Type, Value),
  !.
typecheck(boolean, Value):-
  typecheck(oneof([false,true]), Value).
typecheck(double, Value):-
  typecheck(float, Value),
  !.
typecheck(oneof(Values), Value):-
  memberchk(Value, Values),
  !.
typecheck(Type, Value):-
  must_be(Type, Value),
  !.
% DCG defined types
typecheck(Type, Value):-
  atom_chars(Value, ValueChars),
  Call =.. [Type, ValueChars, []],
  call(Call).

%% is_uri(?Resource:uri) is semidet.

is_uri(Resource):-
  uri_components(
    Resource,
    uri_components(Scheme, Authority, Path, _Search, _Fragment)
  ),
  nonvar(Scheme),
  nonvar(Authority),
  nonvar(Path).
