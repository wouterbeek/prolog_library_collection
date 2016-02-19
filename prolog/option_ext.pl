:- module(
  option_ext,
  [
    flatten_option/2, % +Option:compound
                      % -FlattenedOptions:list(compound)
    if_option/3, % ?Option:compound
                 % +Options:list(compound)
                 % :Goal_0
    if_option/4, % ?Option:compound
                 % +Options:list(compound)
                 % +Default
                 % :Goal_0
    merge_options/2, % +Optionss:list(list(compound))
                     % -Options:list(compound)
    option_components/3, % ?Option:compound
                         % ?Name:atom
                         % ?Value
    option_has_var_value/1, % +Option:compound
    option_pair/2 % ?Option:compound
                  % ?Pair:pair
  ]
).
:- reexport(library(option)).

/** <module> Option extensions

@author Wouter Beek
@version 2015/07, 2015/10-2016/01
*/

:- use_module(library(apply)).
:- use_module(library(yall)).

:- meta_predicate
    if_option(?, +, 0),
    if_option(?, +, +, 0).





%! flatten_option(+Options:compound, -FlattenedOptions:list(compound)) is det.

flatten_option(Opt, Opts):-
  option_components(Opt, N, Vs),
  maplist([V,Opt]>>option_components(Opt, N, V), Vs, Opts).



%! if_option(?Option:compound, +Options:list(compound), :Goal_0) is det.

if_option(Opt, Opts, Goal_0):-
  option(Opt, Opts), !,
  Goal_0.
if_option(_, _, _).


%! if_option(
%!   ?Option:compound,
%!   +Options:list(compound),
%!   +Default,
%!   :Goal_0
%! ) is det.

if_option(Opt, Opts, Default, Goal_0):-
  option(Opt, Opts, Default), !,
  Goal_0.
if_option(_, _, _, _).



%! merge_options(
%!   +Optionss:list(list(compound)),
%!   -Options:list(compound)
%! ) is det.
% Generalization of merge_options/3.

merge_options([H1,H2|T], L):- !,
  merge_options(H1, H2, H3),
  merge_options([H3|T], L).
merge_options([L], L):- !.
merge_options([], []).



%! option_components(+Option:compound, -Name:atom, -Value) is det.
%! option_components(+Option:compound, -Name:atom, -Value) is det.

option_components(Opt, Name, Value):-
  Opt =.. [Name,Value].



%! option_has_var_value(+Option:compound) is semidet.
% Succeeds if the value of Option is uninstantiated.

option_has_var_value(Opt):-
  Opt =.. [_,Val],
  var(Val).



%! option_pair(+Option:compound, +Pair:pair) is semidet.
%! option_pair(+Option:compound, -Pair:pair) is det.
%! option_pair(-Option:compound, +Pair:pair) is det.

option_pair(Opt, N-V):-
  Opt =.. [N,V].
