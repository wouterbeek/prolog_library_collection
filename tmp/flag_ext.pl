:- module(
  flag_ext,
  [
    enable_switch/1, % +Switch:compound
    disable_switch/1, % +Switch:compound
    switch/1, % ?Switch:compound
    switch/2, % +Switch:compound
              % ?Status:boolean
    tmp_set_prolog_flag/3, % +Flag:atom
                           % +Value
                           % :Goal_0
    tmp_set_setting/3 % +Setting:atom
                      % +Value
                      % :Goal_0
  ]
).

/** <module> Flag extensions

Extensions for setting Prolog flags.

@author Wouter Beek
@tbd Support global and local switches.
@version 2015/08, 2015/10, 2015/12
*/

:- use_module(library(db_ext)).
:- use_module(library(settings)).

:- meta_predicate(tmp_set_prolog_flag(+,+,0)).
:- meta_predicate(tmp_set_setting(+,+,0)).

%! switch(+Switch:compound, +Status:boolean) is semidet.
%! switch(+Switch:compound, -Status:boolean) is det.
%! switch(-Switch:compound, +Status:boolean) is nondet.
%! switch(-Switch:compound, -Status:boolean) is nondet.

:- thread_local(switch/2).





%! enable_swistch(+Switch:compound) is det.

enable_switch(Switch):-
  db_replace(switch(Switch,true)).



%! disable_swistch(+Switch:compound) is det.

disable_switch(Switch):-
  db_replace(switch(Switch,false)).



%! switch(+Switch:compound) is semidet.
%! switch(-Switch:compound) is nondet.

switch(Switch):-
  switch(Switch, true).



%! tmp_set_prolog_flag(+Flag:atom, +Value, :Goal_0) is det.
% Set Flag to Value durign a call of Goal_0.

tmp_set_prolog_flag(Flag, Tmp, Goal_0):-
  current_prolog_flag(Flag, Main), !,
  setup_call_cleanup(
    set_prolog_flag(Flag, Tmp),
    Goal_0,
    reset_prolog_flag(Flag, Main, Tmp)
  ).
tmp_set_prolog_flag(Flag, Tmp, Goal_0):-
  create_prolog_flag(Flag, Tmp, []),
  call(Goal_0).

reset_prolog_flag(_, Main, Main):- !.
reset_prolog_flag(Flag, Main, _):-
  set_prolog_flag(Flag, Main).



%! tmp_set_setting(+Setting:atom, +Value, :Goal_0) is det.

tmp_set_setting(Setting, Tmp, Goal_0):-
  strip_module(Goal_0, Mod, _),
  Mod:setting(Setting, Main),
  setup_call_cleanup(
    Mod:set_setting(Setting, Tmp),
    Goal_0,
    Mod:set_setting(Setting, Main)
  ).
