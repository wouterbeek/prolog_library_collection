:- module(
  dlist,
  [
    dappend/2, % +Lists:list(pair(list))
               % -List:pair(list)
    dappend/3, % +List1:pair(list)
               % +List2:pair(list)
               % -List3:pair(list)
    dlist_to_list/2, % +DifferenceList:pair(list)
                     % -List:list
    is_dlist/1 % @Term
  ]
).

/** <module> Difference lists

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(apply)).





%! dappend(+Lists:list(pair(list)), -List:pair(list)) is det.

dappend([], []):- !.
dappend([L-[]], L):- !.
dappend([L1,L2], L):- !,
  dappend(L1, L2, L-[]).
dappend([L1,L2|T], L):-
  dappend(L1, L2, L3),
  dappend([L3|T], L).



%! dappend(+List1:pair(list), +List2:pair(list), -List3:pair(list)) is det.

dappend(L1-H1, H1-H2, L1-H2).



%! dlist_to_list(+DifferenceList:pair(list), -List:list) is det.

dlist_to_list(L1, L):-
  dappend(L1, []-[], L-[]).



%! is_dlist(@Term) is semidet.

is_dlist(L1-L2):-
  maplist(is_list, [L1,L2]).
