:- module(
  option_ext,
  [
    add_option/4, % +Opts1
                  % +Name
                  % +Value
                  % +Opts2
    add_default_option/4, % +Opts1
                          % +Name
                          % +DefaultValue
                          % -Opts2
    add_default_option/5, % +Opts1
                          % +Name
                          % +DefaultValue
                          % -StoredValue
                          % -Opts2
    if_select_option/4, % +Opt
                        % +Opts1
                        % -Opts2
                        % :Goal
    remove_option/4, % +Opts1
                     % +Name
                     % ?Value
                     % -Opts2
    replace_option/5, % +Opts1
                      % +Name
                      % +NewValue
                      % -OldValue
                      % -Opts2
    update_option/4, % +Opts1
                     % +Name
                     % :Goal
                     % -Opts2
    update_option/5 % +Opts1
                    % +Name
                    % :Goal
                    % -OldValue
                    % -Opts2
  ]
).

/** <module> Option list handling extension

Extensions to the swipl buitin handling of option lists.

This module allows the use of default option values in option/3 that have
arbitrary arity. The swipl builtin only handles default values for the
first argument position in the given option term (probably under the
assumption that the option term will always be unary).

@author Wouter Beek
@version 2013/01, 2013/07-2013/08, 2013/11-2013/12, 2014/04, 2014/06-2014/08
*/

:- use_module(library(option)).

:- meta_predicate(if_select_option(+,+,-,0)).
:- meta_predicate(update_option(+,+,2,-)).
:- meta_predicate(update_option(+,+,2,-,-)).



%! add_option(
%!   +Opts1,
%!   +Name:atom,
%!   ?Value:atom,
%!   +Opts2
%! ) is det.
% Adds an option with the given name and value (i.e. `Name(Value)`),
% and ensures that old options are overwritten and
% that the resultant options list is sorted.
%
% @arg Opts1
% @arg Name
% @arg Value If `Value` is not instantiated, `Opts1 = Opts2`.
% @arg Opts2

add_option(Opts, _, X, Opts):-
  var(X), !.
add_option(Opts1, N, V, Opts2):-
  once(nvpair(N, V, Opt)),
  merge_options([Opt], Opts1, Opts2).


%! add_default_option(
%!   +Opts1,
%!   +Name:atom,
%!   +DefaultValue,
%!   -Opts2
%! ) is det.
% @see add_default_option/5

add_default_option(Os1, N, DefaultV, Os2):-
  add_default_option(Os1, N, DefaultV, _StoredV, Os2).


%! add_default_option(
%!   +Opts1,
%!   +Name:atom,
%!   +DefaultValue,
%!   -StoredValue,
%!   -Opts2
%! ) is det.
% Gives either the stored value, if it is available,
% or the given default value.
% Also returns the resultant options list.

add_default_option(Opts1, Name, _DefaultV, StoredV, Opts1):-
  nvpair(Name, StoredV, Opt),
  option(Opt, Opts1), !.
add_default_option(Opts1, Name, DefaultV, DefaultV, Opts2):-
  add_option(Opts1, Name, DefaultV, Opts2).


%! if_select_option(
%!   +Opt:nvpair,
%!   +Opts,
%!   -RestOpts,
%!   :Goal
%! ) is det.

if_select_option(Opt, Opts1, Opts2, Goal):-
  select_option(Opt, Opts1, Opts2), !,
  Goal.
if_select_option(_, Opts, Opts, _).


%! remove_option(
%!   +OldOpts,
%!   +Name:atom,
%!   ?Value,
%!   -NewOpts
%! ) is det.
% Removes at most one option (i.e., if at least one appears)
% with the given name and value from the options list.

remove_option(Opts1, N, V, Opts2):-
  pair(N, V, Opt),
  select_option(Opt, Opts1, Opts2), !.
remove_option(Opts, _, _, Opts).

%! replace_option(
%!   +OldOpts,
%!   +Name:atom,
%!   +NewValue,
%!   -OldValue,
%!   -NewOpts
%! ) is det.

replace_option(Os1, N, V2, V1, Os3):-
  remove_option(Os1, N, V1, Os2),
  add_option(Os2, N, V2, Os3).


%! update_option(
%!   +OldOpts,
%!   +Name:atom,
%!   :Predicate,
%!   -NewOpts
%! ) is det.
% @see Wrapper around update_option/5, not returning the old value.

update_option(Os1, N, Predicate, Os2):-
  update_option(Os1, N, Predicate, _OldV, Os2).

%! update_option(
%!   +OldOpts,
%!   +Name:atom,
%!   :Predicate,
%!   -OldValue,
%!   -NewOpts
%! ) is det.

update_option(Os1, N, Predicate, V1, Os3):-
  remove_option(Os1, N, V1, Os2),
  call(Predicate, V1, V2),
  add_option(Os2, N, V2, Os3).

