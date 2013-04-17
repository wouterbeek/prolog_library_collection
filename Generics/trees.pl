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
@version 2013/04
*/

:- use_module(pgc(list_ext)).
:- use_module(pgc(meta_ext)).



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

some_subpaths_to_tree(SomeSubPaths):-
  setoff(
    SubPath,
    (
      member(SomeSubPath, SomeSubPaths),
      sublist(SubPath, SomeSubPath),
      SubPath \== SomeSubPath
    ),
    AllSubPaths
  ),
  all_subpaths_to_tree(AllSubPaths).

