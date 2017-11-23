:- module(
  call_ext,
  [
    call_bool/2,            % :Goal_0, -Bool
    call_det_when/2,        % :Cond_0, :Goal_0
    call_det_when_ground/1, % :Goal_0
    call_det_when_ground/2, % :Cond_0, :Goal_0
    call_default_option/3,  % ?Option, +Options, :Goal_1
    call_must_be/2,         % :Goal_1, @Term
    call_or_warning/1,      % :Goal_0
    call_pair/3,            % :Goal_2, +Pair1, -Pair2
    call_statistics/3,      % :Goal_0, +Key, -Delta
    call_stats/3,           % :Select_1, :Goal_1, -Stats
    call_stats_n/3,         % +Repeats, :Goal_0, -Stats
    call_when_ground/1,     % :Goal_0
    is_det/1,               % :Goal_0
    true/1,                 % ?Arg1
    true/2,                 % ?Arg1, ?Arg2
    true/3                  % ?Arg1, ?Arg2, ?Arg3
  ]
).

/** <module> Call extensions

@author Wouter Beek
@version 2017/04-2017/11
*/

:- use_module(library(apply)).
:- use_module(library(dif)).
:- use_module(library(option)).
:- use_module(library(when)).

:- meta_predicate
    call_bool(0, -),
    call_default_option(?, +, 1),
    call_det_when(0, 0),
    call_det_when_ground(0),
    call_det_when_ground(?, 0),
    call_must_be(1, +),
    call_or_warning(0),
    call_pair(2, +, -),
    call_statistics(0, +, -),
    call_stats(1, 1, -),
    call_stats_n(+, 0, -),
    call_when_ground(0),
    is_det(0).





%! call_bool(:Goal_0, -Bool) is det.

call_bool(Goal_0, true) :-
  Goal_0, !.
call_bool(_, false).



%! call_default_option(?Option, +Options, :Goal_1) is det.

call_default_option(Option, Options, _) :-
  option(Option, Options), !.
call_default_option(Option, _, Goal_1) :-
  Option =.. [_,Value],
  (call(Goal_1, DefaultValue) -> Value = DefaultValue).



%! call_det_when(:Cond_0, :Goal_0) .
%
% Call `Goal_0' once when `Cond_0' succeeds.  Otherwise call `Goal_0'
% normally.

call_det_when(Cond_0, Goal_0) :-
  Cond_0, !,
  once(Goal_0).
call_det_when(_, Goal_0) :-
  Goal_0.



%! call_det_when_ground(:Goal_0) .
%! call_det_when_ground(+Term:term, :Goal_0) .
%
% Call `Goal_0' deterministically in case Term is ground.  Otherwise
% call `Goal_0' normally.

call_det_when_ground(Mod:Goal_0) :-
  call_det_when_ground(Goal_0, Mod:Goal_0).


call_det_when_ground(Term, Goal_0) :-
  ground(Term), !,
  once(Goal_0).
call_det_when_ground(_, Goal_0) :-
  Goal_0.



%! call_must_be(:Goal_1, @Term) is det.

call_must_be(Goal_1, Term) :-
  findall(Atom, call(Goal_1, Atom), Atoms),
  must_be(oneof(Atoms), Term).



%! call_or_warning(:Goal_0) is semidet.

call_or_warning(Goal_0) :-
  catch(Goal_0, E, true),
  (var(E) -> true ; print_message(warning, E), fail).



%! call_pair(:Goal_2, +Pair1:pair, -Pair2:pair) is det.
%
% Calls Goal_2 on the values of Pair1 and Pair2.

call_pair(Goal_2, Key-Value1, Key-Value2) :-
  call(Goal_2, Value1, Value2).



%! call_statistics(:Goal_0, +Key, -Delta) is det.

call_statistics(Goal_0, Key, Delta):-
  statistics(Key, Val1a),
  fix_val0(Val1a, Val1b),
  call(Goal_0),
  statistics(Key, Val2a),
  fix_val0(Val2a, Val2b),
  Delta is Val2b - Val1b.

fix_val0([X,_], X) :- !.
fix_val0(X, X).



%! call_stats(:Select_1, :Goal_1, -Stats:dict) is det.
%
% _{
%   cputime: float,
%   inferences: nonneg,
%   max: float,
%   min: float,
%   walltime: float
% }

call_stats(Select_1, Goal_1, Stats) :-
  % Initialize the state based on the first run.
  stats(Cpu1, Inf1, Wall1),
  once(call(Select_1, X0)),
  call(Goal_1, X0),
  stats(Cpu2, Inf2, Wall2),
  Cpu12 is Cpu2 - Cpu1,
  Inf12 is Inf2 - Inf1,
  Wall12 is Wall2 - Wall1,
  State = _{
    cputime: Cpu12-1,
    inferences: Inf12-1,
    maxcpu: Cpu12,
    mincpu: Cpu12,
    walltime: Wall12-1
  },
  forall(
    call(Select_1, X),
    (
      stats(Cpu3, Inf3, Wall3),
      call(Goal_1, X),
      stats(Cpu4, Inf4, Wall4),
      Cpu34 is Cpu4 - Cpu3,
      update_average(cputime, State, Cpu34),
      Inf34 is Inf4 - Inf3,
      update_average(inferences, State, Inf34),
      (   get_dict(maxcpu, State, Max),
          Cpu34 > Max
      ->  nb_set_dict(maxcpu, State, Cpu34)
      ;   true
      ),
      (   get_dict(mincpu, State, Min),
          Cpu34 < Min
      ->  nb_set_dict(mincpu, State, Cpu34)
      ;   true
      ),
      Wall34 is Wall4 - Wall3,
      update_average(walltime, State, Wall34)
    )
  ),
  _{
    cputime: Cpu-Repeats,
    inferences: Inf-Repeats,
    maxcpu: Max,
    mincpu: Min,
    walltime: Wall-Repeats
  } :< State,
  Stats = _{
    cputime: Cpu,
    inferences: Inf,
    max: Max,
    min: Min,
    walltime: Wall
  }.



%! call_stats_n(+Repeats:positive_integer, :Goal_0, -Stats:dict) is det.
%
% _{
%   cputime: float,
%   inferences: nonneg,
%   max: float,
%   min: float,
%   walltime: float
% }

call_stats_n(Repeats, Goal_0, Stats) :-
  % Initialize the state based on the first run.
  stats(Cpu1, Inf1, Wall1),
  call(Goal_0),
  stats(Cpu2, Inf2, Wall2),
  Cpu12 is Cpu2 - Cpu1,
  Inf12 is Inf2 - Inf1,
  Wall12 is Wall2 - Wall1,
  State = _{
    cputime: Cpu12-1,
    inferences: Inf12-1,
    maxcpu: Cpu12,
    mincpu: Cpu12,
    walltime: Wall12-1
  },
  forall(
    between(2, Repeats, _),
    (
      stats(Cpu3, Inf3, Wall3),
      call(Goal_0),
      stats(Cpu4, Inf4, Wall4),
      Cpu34 is Cpu4 - Cpu3,
      update_average(cputime, State, Cpu34),
      Inf34 is Inf4 - Inf3,
      update_average(inferences, State, Inf34),
      (   get_dict(maxcpu, State, Max),
          Cpu34 > Max
      ->  nb_set_dict(maxcpu, State, Cpu34)
      ;   true
      ),
      (   get_dict(mincpu, State, Min),
          Cpu34 < Min
      ->  nb_set_dict(mincpu, State, Cpu34)
      ;   true
      ),
      Wall34 is Wall4 - Wall3,
      update_average(walltime, State, Wall34)
    )
  ),
  _{
    cputime: Cpu-Repeats,
    inferences: Inf-Repeats,
    maxcpu: Max,
    mincpu: Min,
    walltime: Wall-Repeats
  } :< State,
  Stats = _{
    cputime: Cpu,
    inferences: Inf,
    max: Max,
    min: Min,
    walltime: Wall
  }.

stats(Cpu, Inf, Wall) :-
  statistics(inferences, Inf),
  statistics(cputime, Cpu),
  get_time(Wall).

update_average(Key, State, Avg) :-
  get_dict(Key, State, M1-N1),
  N2 is N1 + 1,
  M2 is ((N1 * M1) + Avg) / N2,
  nb_set_dict(Key, State, M2-N2).



%! call_when_ground(:Goal_0) is det.

call_when_ground(Goal_0) :-
  when(ground(Goal_0), Goal_0).



%! is_det(:Goal_0) is semidet.

is_det(Goal_0) :-
  call_cleanup(Goal_0, Det = true),
  (Det == true -> true ; !, fail).



%! true(?Arg1) is det.
%! true(?Arg1, ?Arg2) is det.
%! true(?Arg1, ?Arg2, ?Arg3) is det.

true(_).
true(_, _).
true(_, _, _).
