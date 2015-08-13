:- module(
  flag_ext,
  [
    state_init/1, % -State:compound
    state_read/2, % +State:compound
                  % -N:nonneg
    state_tick/1, % +State:compound
    state_tick/2 % +State:compound
                 % -N:nonneg
  ]
).

state_init(state(0)).

state_read(State, N):-
  arg(1, State, N).

state_tick(State):-
  state_tick(State, _).

state_tick(State, C1):-
  arg(1, State, C0),
  C1 is C0 + 1,
  nb_setarg(1, State, C1).
