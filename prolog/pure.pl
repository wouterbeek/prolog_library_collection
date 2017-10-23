:- module(
  pure,
  [
    '='/3,                   % ?X, ?Y, ?Truth
    if_/3,                   % :If_1, :Then_0, :Else_0
    list_item_isMember/3,    % ?L, ?X, ?Truth
    list_list_intersection/3 % ?Xs, ?Ys, ?Zs
  ]
).
:- reexport(library(dif)).

/** <module> Pure
*/

:- meta_predicate
    if_(1, 0, 0).





%! =(?X, ?Y, ?Truth)

=(X, Y, R) :- X == Y,    !, R = true.
=(X, Y, R) :- ?=(X, Y),  !, R = false. % syntactically different
=(X, Y, R) :- X \= Y,    !, R = false. % semantically different
=(X, Y, R) :- R == true, !, X = Y.
=(X, X, true).
=(X, Y, false) :-
   dif(X, Y).



%! if_(:If_1, :Then_0, :Else_0)

if_(C_1, Then_0, Else_0) :-
  call(C_1, Truth),
  functor(Truth, _, 0), % safety check
  (Truth == true -> Then_0 ; Truth == false, Else_0).



%! list_item_isMember(?L, ?X, ?Truth)

list_item_isMember([], _, false).
list_item_isMember([X|Xs], E, Truth) :-
  if_(E = X, Truth = true, list_item_isMember(Xs, E, Truth)).



%! list_list_intersection(?Xs, ?Ys, ?Zs)

list_list_intersection([], _, []).
list_list_intersection([A|As], Bs, Cs1) :-
  if_(list_item_isMember(Bs, A), Cs1 = [A|Cs], Cs1 = Cs),
  list_list_intersection(As, Bs, Cs).
