:- module(
  call_ext,
  [
    call_bool/2,            % :Goal_0, -Bool
    call_det_when/2,        % :Cond_0, :Goal_0
    call_det_when_ground/2, % :Cond_0, :Goal_0
    call_default_option/3,  % ?Option, +Options, :Goal_1
    call_must_be/2,         % :Goal_1, @Term
    call_or_warning/1,      % :Goal_0
    call_statistics/3,      % :Goal_0, +Key, -Delta
    call_when_ground/1,     % :Goal_0
    closure/3,              % :Goal_2, +From, -To
    closure0/3,             % :Goal_2, +From, -To
    is_det/1,               % :Goal_0
    pp_call/1,              % :Goal_1
    true/1,                 % ?Arg1
    true/2,                 % ?Arg1, ?Arg2
    true/3                  % ?Arg1, ?Arg2, ?Arg3
  ]
).

/** <module> Call extensions

@author Wouter Beek
@version 2017/04-2017/09
*/

:- use_module(library(apply)).
:- use_module(library(dif)).
:- use_module(library(option)).
:- use_module(library(pp)).
:- use_module(library(when)).

:- meta_predicate
    call_bool(0, -),
    call_default_option(?, +, 1),
    call_det_when(0, 0),
    call_det_when_ground(?, 0),
    call_must_be(1, +),
    call_or_warning(0),
    call_statistics(0, +, -),
    call_when_ground(0),
    closure(2, +, -),
    closure0(2, +, -),
    closure0(2, +, -, +),
    is_det(0),
    pp_call(1).





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



%! call_det_when_ground(+Term:term, :Goal_0) .
%
% Call `Goal_0' deterministically in case Term is ground.  Otherwise
% call `Goal_0' normally.

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



%! call_when_ground(:Goal_0) is det.

call_when_ground(Goal_0) :-
  when(ground(Goal_0), Goal_0).



%! closure(:Goal_2, +X, -Y) is nondet.
%
% Calculates the transitive closure of `Goal_2`.
%
% @author Ulrich Neumerkel
%
% @see http://stackoverflow.com/questions/26964782/determining-if-graph-is-connected-in-prolog/26965843?noredirect=1#comment42472120_26965843
%
% @see http://stackoverflow.com/questions/15473065/hilog-terms-in-xsb-prolog/15483764#15483764

closure(Goal_2, X, Z):-
  call(Goal_2, X, Y),
  closure0(Goal_2, Y, Z, [X,Y]).



%! closure0(:Goal_2, +X, -Y) is multi.
%
% Calculates the transitive-reflexive closure of `Goal_2`.
%
% @author Ulrich Neumerkel
%
% @see http://stackoverflow.com/questions/26964782/determining-if-graph-is-connected-in-prolog/26965843?noredirect=1#comment42472120_26965843
%
% @see http://stackoverflow.com/questions/26946133/definition-of-reflexive-transitive-closure

closure0(Goal_2, X, Y):-
  closure0(Goal_2, X, Y, [X]).

closure0(_, X, X, _).
closure0(Goal_2, X, Z, Hist):-
  call(Goal_2, X, Y),
  maplist(dif(Y), Hist),
  closure0(Goal_2, Y, Z, [Y|Hist]).



%! is_det(:Goal_0) is semidet.

is_det(Goal_0) :-
  call_cleanup(Goal_0, Det = true),
  (Det == true -> true ; !, fail).



%! pp_call(:Goal_1) is det.

pp_call(Goal_1) :-
  catch(call(Goal_1, Term), E, true),
  (var(E) -> pp_term(Term) ; print_message(warning, E)).



%! true(?Arg1) is det.
%! true(?Arg1, ?Arg2) is det.
%! true(?Arg1, ?Arg2, ?Arg3) is det.

true(_).
true(_, _).
true(_, _, _).
