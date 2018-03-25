:- module(
  default,
  [
    call_default_option/3, % ?Option, +Options, :Goal_1
    call_default_value/2,  % ?Value, :Goal_1
    call_default_value/3,  % ?Value, :Goal_1, +DefaultValue
    default_value/2        % ?Value, +DefaultValue
  ]
).

/** <module> Default

@author Wouter Beek
@version 2017-2018
*/

:- meta_predicate
    call_default_option(?, +, 1),
    call_default_value(?, 1),
    call_default_value(?, 1, +).





%! call_default_option(?Option, +Options, :Goal_1) is det.
%
% If `Option' cannot be bound based on the given `Options', call
% `Goal_1' to determine the option's binding instead.

call_default_option(Option, Options, _) :-
  option(Option, Options), !.
call_default_option(Option, _, Goal_1) :-
  Option =.. [_,Value],
  (call(Goal_1, DefaultValue) -> Value = DefaultValue).



%! call_default_value(?Value, :Goal_1) is det.
%
% If `Value' is not bound, call `Goal_1' to determine its default
% value.

call_default_value(Value, _) :-
  nonvar(Value), !.
call_default_value(Value, Goal_1) :-
  call(Goal_1, Value).


%! call_default_value(?Value, :Goal_1, +DefaultValue) is det.
%
% If `Value' cannot be determined by calling `Goal_1', the
% `DefaultValue' is used instead.

call_default_value(Value, Goal_1, _) :-
  call(Goal_1, Value), !.
call_default_value(DefaultValue, _, DefaultValue).



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
