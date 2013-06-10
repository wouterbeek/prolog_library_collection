%! set(+Name:atom, +Value) is semidet.
% Sets the new value for the setting with the given name.

set(Name,Value):-
  (
    Value = 1e10
  ->
    V is 1e10
  ;
    Value = +1e10
  ->
    V is 1e10
  ;
    Value = -1e10
  ->
    V is -1e10
  ;
    V = Value
  ),
  % Retract the old value.
  retractall('$aleph_global'(Name, set(Name, _OldValue))),
  % Assert the new value.
  assertz('$aleph_global'(Name, set(Name, V))),
  % @tbd This broadcast is never listened to.
  broadcast(set(Name, V)),
  special_consideration(Name, Value).

