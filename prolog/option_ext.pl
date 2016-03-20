:- module(
  option_ext,
  [
    flatten_option/2,       % +Opt, -FlattenedOpts
    if_option/3,            % ?Opt, +Opts, :Goal_0
    if_option/4,            % ?Opt, +Opts, +Default, :Goal_0
    merge_options/2,        % +Optss:list(list), -Opts:list
    option_components/3,    % ?Opt, ?Name, ?Value
    option_has_var_value/1, % +Opt
    option_pair/2           % ?Opt, ?Pair
  ]
).
:- reexport(library(option)).

/** <module> Opt extensions

@author Wouter Beek
@version 2015/07, 2015/10-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- meta_predicate
    if_option(?, +, 0),
    if_option(?, +, +, 0).





%! flatten_option(+Opts, -FlattenedOpts:list(compound)) is det.

flatten_option(Opt, Opts) :-
  option_components(Opt, N, Vs),
  maplist([V,Opt]>>option_components(Opt, N, V), Vs, Opts).



%! if_option(?Opt, +Opts, :Goal_0) is det.

if_option(Opt, Opts, Goal_0) :-
  option(Opt, Opts), !,
  Goal_0.
if_option(_, _, _).


%! if_option(?Opt, +Opts, +Default, :Goal_0) is det.

if_option(Opt, Opts, Default, Goal_0) :-
  option(Opt, Opts, Default), !,
  Goal_0.
if_option(_, _, _, _).



%! merge_options(+Optss:list(list)), -Opts:list) is det.
% Generalization of merge_options/3.

merge_options([H1,H2|T], L) :- !,
  merge_options(H1, H2, H3),
  merge_options([H3|T], L).
merge_options([L], L) :- !.
merge_options([], []).



%! option_components(+Opt, -Name, -Value) is det.
%! option_components(+Opt, -Name, -Value) is det.

option_components(Opt, Name, Value) :-
  Opt =.. [Name,Value].



%! option_has_var_value(+Opt) is semidet.
% Succeeds if the value of Opt is uninstantiated.

option_has_var_value(Opt) :-
  Opt =.. [_,Val],
  var(Val).



%! option_pair(+Opt, +Pair) is semidet.
%! option_pair(+Opt, -Pair) is det.
%! option_pair(-Opt, +Pair) is det.

option_pair(Opt, N-V) :-
  Opt =.. [N,V].
