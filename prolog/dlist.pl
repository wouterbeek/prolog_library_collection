:- module(
  dlist,
  [
    dappend/2,         % +Ls:list(dlist), -L:dlist
    dappend/3,         % +L1:dlist, +L2:dlist, -L3:dlist
    dappend_to_list/2, % +DLs:list(dlist), -L:list
    dappend_to_list/3, % +DL1:dlist, +DL2:dlist, -L:list
    dlist_to_list/2,   % +DL:dlist, -L:list
    is_dlist/1         % @Term
  ]
).

/** <module> Difference lists

@author Wouter Beek
@version 2015-2018
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



%! dappend_to_list(+DLs:list(dlist), -L:list) is det.
%
% Append an arbitrary number of difference lists into one regular
% list.

dappend_to_list(DLs, L) :-
  dappend(DLs, DL),
  dlist_to_list(DL, L).



%! dappend_to_list(+DL1:dlist, +DL2:dlist, -L:list) is det.
%
% Append two difference lists into one regular list.

dappend_to_list(DL1, DL2, L):-
  dappend(DL1, DL2, DL),
  dlist_to_list(DL, L).



%! dlist_to_list(+DL:dlist, -L:list) is det.
%
% Converts a difference list into a reggular list.

dlist_to_list(L1, L):-
  dappend(L1, []-[], L-[]).



%! is_dlist(@Term) is semidet.
%
% Succeeds if `Term' is a difference list.

is_dlist(L1-L2):-
  maplist(is_list, [L1,L2]).
