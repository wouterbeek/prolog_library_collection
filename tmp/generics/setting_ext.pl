:- module(
  setting_ext,
  [
    temporarily_set_setting/3 % +Name:atom
                              % +Value
                              % :Goal
  ]
).

/** <module> Setting extensions

Additional support for settings in SWI-Prolog.

@author Wouter Beek
@version 2014/07
*/

:- use_module(library(settings)).

:- meta_predicate(temporarily_set_setting(+,+,0)).



temporarily_set_setting(Name, TmpValue, Goal):-
  strip_module(Goal, Module, _),
  Module:setting(Name, Value),
  setup_call_cleanup(
    Module:set_setting(Name, TmpValue),
    call(Goal),
    Module:set_setting(Name, Value)
  ).

