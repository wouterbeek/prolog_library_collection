:- module(
  dlist,
  [
    dappend/2, % +DifferenceLists:list(dlist)
               % -DifferenceList:dlist
    dappend/3, % +DifferenceList1:dlist
               % +DifferenceList2:dlist
               % -DifferenceList3:dlist
    dlist_to_list/2, % +DifferenceList:dlist
                     % -List:list
    is_dlist/1, % @Term
    lappend/2, % +DifferenceLists:list(dlist)
               % -List:list
    lappend/3 % +DifferenceList1:dlist
              % +DifferenceList2:dlist
              % -List:list
  ]
).

/** <module> Difference lists

@author Wouter Beek
@version 2015/08, 2015/11
*/

:- use_module(library(apply)).





%! dappend(+Lists:list(dlist), -List:dlist) is det.

dappend([], []):- !.
dappend([L], L):- !.
dappend([L1,L2], L):- !, dappend(L1, L2, L).
dappend([L1,L2|T], L):- dappend(L1, L2, L3), dappend([L3|T], L).


%! dappend(+List1:dlist, +List2:dlist, -List3:dlist) is det.

dappend(L1-H1, H1-H2, L1-H2).



%! dlist_to_list(+DifferenceList:dlist, -List:list) is det.

dlist_to_list(L1, L):- dappend(L1, []-[], L-[]).



%! is_dlist(@Term) is semidet.

is_dlist(L1-L2):- maplist(is_list, [L1,L2]).



%! lappend(+DifferenceLists:list(dlist), -List:list) is det.

lappend(DLs, L):- dappend(DLs, DL), dlist_to_list(DL, L).


%! lappend(+DifferenceList1:dlist, +DifferenceList2:dlist, -List:list) is det.

lappend(DL1, DL2, L):- dappend(DL1, DL2, DL), dlist_to_list(DL, L).
