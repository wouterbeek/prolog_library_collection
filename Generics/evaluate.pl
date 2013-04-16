:- module(
  evaluate,
  [
    aspect/1, % ?Aspect:atom
    aspect/2, % ?Process:atom
              % ?Aspect:atom
    evaluate_aspect/2, % +Process:atom
                       % +Aspect:atom
    evaluate_process/1, % +Process:atom
    process/1, % ?Process:atom
    register_aspect/2, % +Process:atom
                       % +Aspect:atom
    register_aspect/3, % +Process:atom
                       % +Aspect:atom
                       % +Potential:integer
    register_process/1, % +Process:atom
    succ_actual/2, % +Process:atom
                   % +Aspect:atom
    succ_both/2, % +Process:atom
                 % +Aspect:atom
    succ_potential/2 % +Process:atom
                     % +Aspect:atom
  ]
).

/** <module> Evaluate

Evaluate the results of algorithms.

@author Wouter Beek
@version 2013/01
*/

:- dynamic(process(_Name, _Aspects)).



aspect(Aspect):-
  aspect(_Process, Aspect).

aspect(Process, Aspect):-
  process(Process, Aspects),
  member(Aspect, Aspects).

evaluate_aspect(Process, _Aspect):-
  \+ process(Process, _Aspects),
  !,
  debug(evaluate, 'Process ~w does not exist.', [Process]).
evaluate_aspect(Process, Aspect):-
  process(Process, Aspects),
  \+ member(Aspect, Aspects),
  !,
  debug(
    evaluate,
    'Aspect ~w is not registered with process ~w.',
    [Aspect, Process]
  ).
evaluate_aspect(Process, Aspect):-
  process(Process, Aspects),
  member(Aspect, Aspects),
  format(atom(ActualFlag), '~w_~w_actual', [Process, Aspect]),
  flag(ActualFlag, Actual, Actual),
  format(atom(PotentialFlag), '~w_~w_potential', [Process, Aspect]),
  flag(PotentialFlag, Potential, Potential),
  (
    Potential == 0
  ->
    Percentage = 0
  ;
    Percentage is Actual / Potential * 100
  ),
  debug(
    evaluate,
    'Actual: ~w\tPotential:~w\tSuccess:~w%.',
    [Actual, Potential, Percentage]
  ).

evaluate_process(Process):-
  forall(
    aspect(Process, Aspect),
    (
      debug(evaluate, 'Process ~w\tAspect ~w', [Process, Aspect]),
      evaluate_aspect(Process, Aspect)
    )
  ).

process(Process):-
  process(Process, _Aspects).

register_aspect(Process, Aspect):-
  register_aspect(Process, Aspect, 0).

register_aspect(Process, Aspect, Potential):-
  \+ process(Process, _Aspects),
  !,
  register_process(Process),
  register_aspect(Process, Aspect, Potential).
register_aspect(Process, Aspect, _Potential):-
  process(Process, Aspects),
  member(Aspect, Aspects),
  !,
  debug(
    evaluate,
    'Aspect ~w already registered with process ~w.',
    [Aspect, Process]
  ).
register_aspect(Process, Aspect, Potential):-
  retract(process(Process, Aspects)),
  assert(process(Process, [Aspect | Aspects])),
  format(atom(ActualFlag), '~w_~w_actual', [Process, Aspect]),
  flag(ActualFlag, _OldActual, 0),
  format(atom(PotentialFlag), '~w_~w_potential', [Process, Aspect]),
  flag(PotentialFlag, _OldPotential, Potential).

register_process(Process):-
  assert(process(Process, [])).

reset_aspect(Process, Aspect):-
  format(atom(ActualFlag), '~w_~w_actual', [Process, Aspect]),
  flag(ActualFlag, _OldActual, 0),
  format(atom(PotentialFlag), '~w_~w_potential', [Process, Aspect]),
  flag(PotentialFlag, _OldPotential, 0),
  retract(process(Process, Aspects)),
  select(Aspect, Aspects, NewAspects),
  assert(process(Process, NewAspects)).

reset_process(Process):-
  forall(
    aspect(Process, Aspect),
    reset_aspect(Process, Aspect)
  ),
  retract(process(Process, [])).

succ_actual(Process, Aspect):-
  format(atom(ActualFlag), '~w_~w_actual', [Process, Aspect]),
  flag(ActualFlag, Actual, Actual + 1).

succ_both(Process, Aspect):-
  succ_actual(Process, Aspect),
  succ_potential(Process, Aspect).

succ_potential(Process, Aspect):-
  format(atom(ActualFlag), '~w_~w_potential', [Process, Aspect]),
  flag(ActualFlag, Actual, Actual + 1).

