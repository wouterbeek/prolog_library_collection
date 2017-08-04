:- module(
  default,
  [
    defval/2 % +DefaulValue, ?Value
  ]
).

/** <module> Default

@author Wouter Beek
@version 2017/06
*/





%! defval(+DefaultValue, ?Value) is det.
%
% Returns either the given value or the default value, in case there
% is no value given.
%
% ### Example
%
% `Ordering` is a meta-argument that allows a list of elements to be
% arbitrarily ordered.  The use of defval/2 here allows the original
% ordering of elements to be retained in case the `Ordering` argument
% is not instantiated.
%
% ```prolog
% defval(=, Ordering),
% once(call(Ordering, L1, L2))
% ```

defval(_, Value):-
  nonvar(Value), !.
defval(DefaultValue, DefaultValue).
