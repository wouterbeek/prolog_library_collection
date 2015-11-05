:- module(
   direct_subset,
   [
    direct_subset/3 % +Sets:ordset(ordset)
                    % -Set1:ordset
                    % -Set2:ordset
  ]
).

/** <module> Direct subset

@author Wouter Beek
@version 2015/11
*/

:- use_module(library(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(set/set_ext)).




%! direct_subset(+Sets:ordset(ordset), ?Set1:ordset, ?Set2:ordset) is nondet.

direct_subset(Sets, A, B):-
  % NONDET
  split_member(Sets, _, A, AfterA),
  % NONDET
  split_member(AfterA, BetweenAB, B, _),
  strict_subset(A, B),
  \+ (member(C, BetweenAB), strict_subset(A, C), strict_subset(C, B)).
