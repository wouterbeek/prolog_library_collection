:- module(
  dcg_call,
  [
    dcg_apply//2, % :Dcg, +Args:list
    dcg_apply_cp//2, % :Dcg, +Args:list
    dcg_between//2, % :Between_0, :Dcg_0
    dcg_between//3, % :Begin_0, :Dcg_0, :End_0
    dcg_call//1, % :Dcg_0
    dcg_call//2, % :Dcg_1, ?Arg1
    dcg_call//3, % :Dcg_2, ?Arg1, ?Arg2
    dcg_call//4, % :Dcg_3, ?Arg1, ?Arg2, ?Arg3
    dcg_call//5, % :Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4
    dcg_call//6, % :Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5
    dcg_call_cp//1, % :Dcg_0
    dcg_call_cp//2, % :Dcg_1, ?Arg1
    dcg_call_cp//3, % :Dcg_2, ?Arg1, ?Arg2
    dcg_call_cp//4, % :Dcg_3, ?Arg1, ?Arg2, ?Arg3
    dcg_call_cp//5, % :Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4
    dcg_call_cp//6, % :Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5
    dcg_once//1 % :Dcg_0
  ]
).

/** <module> DCG call

Variants of apply/2, call/[1,5], once/1 for DCGs.

@author Wouter Beek
@version 2015/07
*/

:- use_module(library(lists)).

:- meta_predicate(dcg_apply(//,+,?,?)).
:- meta_predicate(dcg_apply_cp(//,+,?,?)).
:- meta_predicate(dcg_between(//,//,?,?)).
:- meta_predicate(dcg_between(//,//,//,?,?)).
:- meta_predicate(dcg_call(//,?,?)).
:- meta_predicate(dcg_call(3,?,?,?)).
:- meta_predicate(dcg_call(4,?,?,?,?)).
:- meta_predicate(dcg_call(5,?,?,?,?,?)).
:- meta_predicate(dcg_call(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_call(7,?,?,?,?,?,?,?)).
:- meta_predicate(dcg_call_cp(//,?,?)).
:- meta_predicate(dcg_call_cp(3,?,?,?)).
:- meta_predicate(dcg_call_cp(4,?,?,?,?)).
:- meta_predicate(dcg_call_cp(5,?,?,?,?,?)).
:- meta_predicate(dcg_call_cp(6,?,?,?,?,?,?)).
:- meta_predicate(dcg_call_cp(7,?,?,?,?,?,?,?)).
:- meta_predicate(dcg_once(//,?,?)).





%! dcg_apply(:Dcg, +Arguments:list)// .
% Variant of apply/2 for DCGs.

dcg_apply(Dcg, Args1, X, Y):-
  append(Args1, [X,Y], Args2),
  apply(Dcg, Args2).



%! dcg_apply_cp(:Dcg, +Arguments:list)// .
% Variant of dcg_apply/2 where copies of Dcg are called.

dcg_apply_cp(Dcg, Args1, X, Y):-
  copy_term(Dcg, Dcg_),
  append(Args1, [X,Y], Args2),
  apply(Dcg_, Args2).



%! dcg_between(:Between_0, :Dcg_0)// .

dcg_between(Between_0, Dcg_0) -->
  dcg_between(Between_0, Dcg_0, Between_0).


%! dcg_between(:Begin_0, :Dcg_0, :End_0)// .

dcg_between(Begin_0, Dcg_0, End_0) -->
  Begin_0,
  Dcg_0,
  End_0.



%! dcg_call(:Dcg_0)// .
%! dcg_call(:Dcg_1, ?Arg1)// .
%! dcg_call(:Dcg_2, ?Arg1, ?Arg2)// .
%! dcg_call(:Dcg_3, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call(:Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call(:Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
% Dcg is called directly (i.e., not copied).
% This means that multiple calls of the same Dcg may share variables.
%
% This is a DCG-based  variant of call//[1-5].

dcg_call(Dcg_0, X, Y):-
  call(Dcg_0, X, Y).

dcg_call(Dcg_1, A1, X, Y):-
  call(Dcg_1, A1, X, Y).

dcg_call(Dcg_2, A1, A2, X, Y):-
  call(Dcg_2, A1, A2, X, Y).

dcg_call(Dcg_3, A1, A2, A3, X, Y):-
  call(Dcg_3, A1, A2, A3, X, Y).

dcg_call(Dcg_4, A1, A2, A3, A4, X, Y):-
  call(Dcg_4, A1, A2, A3, A4, X, Y).

dcg_call(Dcg_5, A1, A2, A3, A4, A5, X, Y):-
  call(Dcg_5, A1, A2, A3, A4, A5, X, Y).



%! dcg_call_cp(:Dcg_0)// .
%! dcg_call_cp(:Dcg_1, ?Arg1)// .
%! dcg_call_cp(:Dcg_2, ?Arg1, ?Arg2)// .
%! dcg_call_cp(:Dcg_3, ?Arg1, ?Arg2, ?Arg3)// .
%! dcg_call_cp(:Dcg_4, ?Arg1, ?Arg2, ?Arg3, ?Arg4)// .
%! dcg_call_cp(:Dcg_5, ?Arg1, ?Arg2, ?Arg3, ?Arg4, ?Arg5)// .
% Variant of dcg_call//[0-5] where copies of Dcg are called.
%
% dcg_call_cp//1 is included for consistency, even though
% it is operationally equivalent to dcg_call//1.

dcg_call_cp(Dcg_0, X, Y):-
  copy_term(Dcg_0, Dcg__0),
  call(Dcg__0, X, Y).

dcg_call_cp(Dcg_1, A1, X, Y):-
  copy_term(Dcg_1, Dcg__1),
  call(Dcg__1, A1, X, Y).

dcg_call_cp(Dcg_2, A1, A2, X, Y):-
  copy_term(Dcg_2, Dcg__2),
  call(Dcg__2, A1, A2, X, Y).

dcg_call_cp(Dcg_3, A1, A2, A3, X, Y):-
  copy_term(Dcg_3, Dcg__3),
  call(Dcg__3, A1, A2, A3, X, Y).

dcg_call_cp(Dcg_4, A1, A2, A3, A4, X, Y):-
  copy_term(Dcg_4, Dcg__4),
  call(Dcg__4, A1, A2, A3, A4, X, Y).

dcg_call_cp(Dcg_5, A1, A2, A3, A4, A5, X, Y):-
  copy_term(Dcg_5, Dcg__5),
  call(Dcg__5, A1, A2, A3, A4, A5, X, Y).



%! dcg_once(:Dcg_0)// .
% Calls the given DCG at most one time.

dcg_once(Dcg_0, X, Y):-
  once(phrase(Dcg_0, X, Y)).
