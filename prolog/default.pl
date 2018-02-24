:- module(
  default,
  [
    call_default/2,        % ?Value, :Goal_1
    call_default_option/3, % ?Option, +Options, :Goal_1
    default_value/2               % +DefaultValue, ?Value
  ]
).

/** <module> Default

@author Wouter Beek
@version 2017-2018
*/

:- meta_predicate
    call_default(?, 1),
    call_default_option(?, +, 1).





%! call_default(?Value, :Goal_1) is det.

call_default(Value, _) :-
  nonvar(Value), !.
call_default(Value, Goal_1) :-
  call(Goal_1, Value).



%! call_default_option(?Option, +Options, :Goal_1) is det.

call_default_option(Option, Options, _) :-
  option(Option, Options), !.
call_default_option(Option, _, Goal_1) :-
  Option =.. [_,Value],
  (call(Goal_1, DefaultValue) -> Value = DefaultValue).



%! default_value(?Value, +DefaultValue) is det.
%
% Returns either the given value or the default value, in case there
% is no value given.
%
% # Example
%
% `Ordering` is a meta-argument that allows a list of elements to be
% arbitrarily ordered.  The use of default_value/2 here allows the
% original ordering of elements to be retained in case the `Ordering`
% argument is not instantiated.
%
% ```prolog
% default_value(=, Ordering),
% once(call(Ordering, L1, L2))
% ```

default_value(Value, _):-
  nonvar(Value), !.
default_value(DefaultValue, DefaultValue).
