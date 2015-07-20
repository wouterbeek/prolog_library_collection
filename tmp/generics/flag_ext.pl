:- module(
  flag_ext,
  [
    state_init/1, % -State:compound
    state_read/2, % +State:compound
                  % -N:nonneg
    state_tick/1, % +State:compound
    state_tick/2, % +State:compound
                  % -N:nonneg
    tmp_set_prolog_flag/3 % +Flag:atom
                          % +TmpValue
                          % :Goal
  ]
).

/** <module> Flag extensions

Extensions for flags and shared variables.

@author Wouter Beek
@version 2014/06-2014/07, 2015/06
*/

:- meta_predicate(tmp_set_prolog_flag(+,+,0)).





state_init(state(0)).



state_read(State, N):-
  arg(1, State, N).



state_tick(State):-
  state_tick(State, _).

state_tick(State, C1):-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).



%! tmp_set_prolog_flag(+Flag:atom, +TmpValue, :Goal) is det.

tmp_set_prolog_flag(Flag, Tmp, Goal):-
  current_prolog_flag(Flag, Main), !,
  setup_call_cleanup(
    set_prolog_flag(Flag, Tmp),
    Goal,
    reset_prolog_flag(Flag, Main, Tmp)
  ).
tmp_set_prolog_flag(Flag, Tmp, Goal):-
  create_prolog_flag(Flag, Tmp, []),
  call(Goal).

reset_prolog_flag(_, Main, Main):- !.
reset_prolog_flag(Flag, Main, _):-
  set_prolog_flag(Flag, Main).
