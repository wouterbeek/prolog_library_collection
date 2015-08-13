:- module(
  flag_ext,
  [
    tmp_set_prolog_flag/3 % +Flag:atom
                          % +Value
                          % :Goal_0
  ]
).

/** <module> Flag extensions

Extensions for setting Prolog flags.

@author Wouter Beek
@version 2015/08
*/

:- meta_predicate(tmp_set_prolog_flag(+,+,0)).





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
