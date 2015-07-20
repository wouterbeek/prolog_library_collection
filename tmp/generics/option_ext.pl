:- module(
  option_ext,
  [
    add_option/4, % +FromOptions:list(nvpair)
                  % +Name:atom
                  % +Value:atom
                  % +ToOptions:list(nvpair)
    add_default_option/4, % +Os1:list(nvpair)
                          % +Name:atom
                          % +DefaultValue
                          % -Os2:list(nvpair)
    add_default_option/5, % +Os1:list(nvpair)
                          % +Name:atom
                          % +DefaultValue
                          % -StoredValue
                          % -Os2:list(nvpair)
    if_select_option/4, % +Option:nvpair
                        % +Options:list(nvpair)
                        % -RestOptions:list(nvpair)
                        % :Goal
    remove_option/4, % +OldOptions:list(nvpair)
                     % +Name:atom
                     % ?Value
                     % -NewOptions:list(nvpair)
    replace_option/5, % +OldOptions:list(nvpair)
                      % +Name:atom
                      % +NewValue
                      % -OldValue
                      % -NewOptions:list(nvpair)
    update_option/4, % +OldOptions:list(nvpair)
                     % +Name:atom
                     % :Predicate
                     % -NewOptions:list(nvpair)
    update_option/5 % +OldOptions:list(nvpair)
                    % +Name:atom
                    % :Predicate
                    % -OldValue
                    % -OldOptions:list(nvpair)
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

:- use_module(plc(generics/nvpair_ext)).

:- meta_predicate(if_select_option(+,+,-,0)).
:- meta_predicate(update_option(+,+,2,-)).
:- meta_predicate(update_option(+,+,2,-,-)).



%! add_option(
%!   +FromOptions:list(nvpair),
%!   +Name:atom,
%!   ?Value:atom,
%!   +ToOptions:list(nvpair)
%! ) is det.
% Adds an option with the given name and value (i.e. `Name(Value)`),
% and ensures that old options are overwritten and
% that the resultant options list is sorted.
%
% @arg Options1
% @arg Name
% @arg Value If `Value` is not instantiated, `Options1 = Options2`.
% @arg Options2

add_option(Options, _, X, Options):-
  var(X), !.
add_option(Options1, N, V, Options2):-
  once(nvpair(N, V, Option)),
  merge_options([Option], Options1, Options2).


%! add_default_option(
%!   +FromOptions:list(nvpair),
%!   +Name:atom,
%!   +DefaultValue,
%!   -ToOptions:list(nvpair)
%! ) is det.
% @see add_default_option/5

add_default_option(Os1, N, DefaultV, Os2):-
  add_default_option(Os1, N, DefaultV, _StoredV, Os2).


%! add_default_option(
%!   +FromOptions:list(nvpair),
%!   +Name:atom,
%!   +DefaultValue,
%!   -StoredValue,
%!   -ToOptions:list(nvpair)
%! ) is det.
% Gives either the stored value, if it is available,
% or the given default value.
% Also returns the resultant options list.

add_default_option(Options1, Name, _DefaultV, StoredV, Options1):-
  nvpair(Name, StoredV, Option),
  option(Option, Options1), !.
add_default_option(Options1, Name, DefaultV, DefaultV, Options2):-
  add_option(Options1, Name, DefaultV, Options2).


%! if_select_option(
%!   +Option:nvpair,
%!   +Options:list(nvpair),
%!   -RestOptions:list(nvpair),
%!   :Goal
%! ) is det.

if_select_option(Option, Options1, Options2, Goal):-
  select_option(Option, Options1, Options2), !,
  Goal.
if_select_option(_, Options, Options, _).


%! remove_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   ?Value,
%!   -NewOptions:list(nvpair)
%! ) is det.
% Removes at most one option (i.e., if at least one appears)
% with the given name and value from the options list.

remove_option(Options1, N, V, Options2):-
  nvpair(N, V, Option),
  select_option(Option, Options1, Options2), !.
remove_option(Options, _, _, Options).

%! replace_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   +NewValue,
%!   -OldValue,
%!   -NewOptions:list(nvpair)
%! ) is det.

replace_option(Os1, N, V2, V1, Os3):-
  remove_option(Os1, N, V1, Os2),
  add_option(Os2, N, V2, Os3).


%! update_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   :Predicate,
%!   -NewOptions:list(nvpair)
%! ) is det.
% @see Wrapper around update_option/5, not returning the old value.

update_option(Os1, N, Predicate, Os2):-
  update_option(Os1, N, Predicate, _OldV, Os2).

%! update_option(
%!   +OldOptions:list(nvpair),
%!   +Name:atom,
%!   :Predicate,
%!   -OldValue,
%!   -NewOptions:list(nvpair)
%! ) is det.

update_option(Os1, N, Predicate, V1, Os3):-
  remove_option(Os1, N, V1, Os2),
  call(Predicate, V1, V2),
  add_option(Os2, N, V2, Os3).

