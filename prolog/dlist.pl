:- module(
  dlist,
  [
    dappend/2,       % +Ls:list(dlist), -L:dlist
    dappend/3,       % +L1:dlist, % +L2:dlist, % -L3:dlist
    dlist_to_list/2, % +L:dlist, -L:list
    is_dlist/1,      % @Term
    lappend/2,       % +Ls:list(dlist), -L:list
    lappend/3        % +L1:dlist, +L2:dlist, -L:list
  ]
).

/** <module> Difference lists

@author Wouter Beek
@version 2015/08, 2015/11, 2016/03
*/

:- use_module(library(apply)).





%! dappend(+Ls:list(dlist), -L:dlist) is det.

dappend([], []):- !.
dappend([L], L):- !.
dappend([L1,L2], L):- !,
  dappend(L1, L2, L).
dappend([L1,L2|T], L):-
  dappend(L1, L2, L3),
  dappend([L3|T], L).



%! dappend(+L1:dlist, +L2:dlist, -L3:dlist) is det.

dappend(L1-H1, H1-H2, L1-H2).



%! dlist_to_list(+L:dlist, -L:list) is det.

dlist_to_list(L1, L):-
  dappend(L1, []-[], L-[]).



%! is_dlist(@Term) is semidet.

is_dlist(L1-L2):-
  maplist(is_list, [L1,L2]).



%! lappend(+Ls:list(dlist), -L:list) is det.

lappend(DLs, L):-
  dappend(DLs, DL),
  dlist_to_list(DL, L).



%! lappend(+L1:dlist, +L2:dlist, -L:list) is det.

lappend(DL1, DL2, L):-
  dappend(DL1, DL2, DL),
  dlist_to_list(DL, L).
