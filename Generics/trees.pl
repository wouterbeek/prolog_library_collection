:- module(
  trees,
  [
    all_subpaths_to_tree/2, % +AllSubPaths:list(list)
                            % -Tree:tree
    some_subpaths_to_tree/2 % +SomeSubPaths:list(list)
                            % -Tree:tree
  ]
).

/** <module> TREES

@author Wouter Beek
@tbd Test this module.
@version 2013/04-2013/05
*/

:- use_module(generics(list_ext)).
:- use_module(generics(meta_ext)).



all_subpaths_to_tree(Subpaths, [Trees]):-
  all_subpaths_to_tree(Subpaths, [], Trees).

all_subpaths_to_tree(Lists, List, List-Trees):-
  setoff(
    Tree,
    (
      member(LongerList, Lists),
      append(List, [_], LongerList),
      all_subpaths_to_tree(Lists, LongerList, Tree)
    ),
    Trees
  ).

some_subpaths_to_tree(SomeSubPaths, Tree):-
  setoff(
    SubPath,
    (
      member(SomeSubPath, SomeSubPaths),
      sublist(SubPath, SomeSubPath),
      SubPath \== SomeSubPath
    ),
    AllSubPaths
  ),
  all_subpaths_to_tree(AllSubPaths, Tree).

