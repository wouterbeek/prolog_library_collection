:- module(
  tree,
  [
    all_subpaths_to_tree/2, % +AllSubPaths:list(list)
                            % -Tree:tree
    edges_to_root/2, % +Edges:list(compound)
                     % -Root:term
    edges_to_tree/2, % +Edges:list(compound)
                     % -Tree:compound
    print_tree/1, % +Tree
    some_subpaths_to_tree/2, % +SomeSubPaths:list(list)
                             % -Tree:compound
    tree_depth/2, % +Tree:compound
                  % -Depth:nonneg
    tree_to_leaf_coord/2 % +Tree:compound
                         % -Coord:list(nonneg)
  ]
).

/** <module> Tree

Tree datastructure.

### Tree representation

Trees are represented in the following way:
  - Leaf nodes are presented as `t(Node, [])` for unlabeled
    or as `t(Node, _, [])` for labeled trees.
  - Non-leaf nodes are represented as `t(Node, SubTrees)` for unlabeled
    or as `t(Node, Label, SubTrees)` for labeled trees.

A simple example:

```prolog
t(a, [t(b, []), t(c, [])])
```

The above corresponds to the following hierarchical tree visualization.

```
a
|-b
|-c
```

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(dcg/dcg_tree)).
:- use_module(library(list_ext)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).





%! all_subpaths_to_tree(+Subpaths:list(list), -Tree:compound) is det.
% Creates the tree based on the given subpaths.
%
% ### Examples
%
% ```prolog
% ?- all_subpaths_to_tree([[a,b],[a,c]], T).
% T = t(a, [t(b, []), t(c, [])]).
%
% ?- all_subpaths_to_tree([[a,b],[a,c],[a,d]], T).
% T = t(a, [t(b,[]), t(c, []), t(d, [])]).
%
% ?- all_subpaths_to_tree([[a,b],[a,c,e],[a,d]], T).
% T = t(a, [t(b, []), t(c, [t(e, [])]), t(d, [])]).
%
% ?- all_subpaths_to_tree([[a,b],[a,c,e],[a,c,f],[a,d]], T).
% T = t(a, [t(b, []), t(c, [t(e, []), t(f, [])]), t(d, [])]).
% ```

all_subpaths_to_tree(Subpaths, Tree):-
  all_subpaths_to_tree0(Subpaths, [Tree]), !.
all_subpaths_to_tree(_, []).

% Only the empty tree has zero subpaths.
all_subpaths_to_tree0([], []):- !.
% The subpaths may have different lengths.
% If one subpath is fully processed, we still need to process the rest.
all_subpaths_to_tree0([[]|Ls], Tree):- !,
  all_subpaths_to_tree(Ls, Tree).
% Create the (sub)trees for the given subpaths recursively.
all_subpaths_to_tree0(Ls1, Trees):-
  % Groups paths by parent node.
  findall(
    H-T,
    member([H|T], Ls1),
    Pairs1
  ),
  group_pairs_by_key(Pairs1, Pairs2),

  % For each parent node, construct its subtrees.
  findall(
    t(H, Subtrees),
    (
      member(H-Ls2, Pairs2),
      all_subpaths_to_tree0(Ls2, Subtrees)
    ),
    Trees
  ).



%! edges_to_root(+Edges:list(compound), -Root:term) is semidet.

edges_to_root(Es, Root):-
  member(Root-_, Es),
  \+ member(_-Root, Es),
  % The root is unique.
  \+ (
    member(SecondRoot-_, Es),
    \+ member(_-SecondRoot, Es),
    SecondRoot \== Root
  ), !.



%! edges_to_tree(+Edges:list(compound), -Tree:compound) is det.

edges_to_tree([], []):- !.
edges_to_tree(Es, Tree):-
  edges_to_root(Es, Root),
  edges_to_tree(Root, Tree, Es, []).

edges_to_tree(Parent, t(Parent,SubTrees), Es1, Es3):-
  edges_to_children(Parent, Children, Es1, Es2),
  foldl(edges_to_tree, Children, SubTrees, Es2, Es3).

edges_to_children(Parent, [H|T], Es1, Es3):-
  selectchk(edge(Parent,_,H), Es1, Es2), !,
  edges_to_children(Parent, T, Es2, Es3).
edges_to_children(_, [], Es, Es).



print_tree(Tree):-
  dcg_with_output_to(current_output, dcg_tree(Tree)).



some_subpaths_to_tree(SomeSubPaths, Tree):-
  aggregate_all(
    set(SubPath),
    (
      member(SomeSubPath, SomeSubPaths),
      sublist(SubPath, SomeSubPath),
      SubPath \== SomeSubPath
    ),
    AllSubPaths
  ),
  all_subpaths_to_tree(AllSubPaths, Tree).



%! tree_depth(+Tree:compound, -Depth:nonneg) is det.

tree_depth(_-[], 0):- !.
tree_depth(_-SubTs, D):-
  maplist(tree_depth, SubTs, Ds),
  max_list(Ds, MaxD),
  D is MaxD + 1.



%! tree_to_leaf_coord(+Tree:compound, -Coord:list(nonneg)) is nondet.

tree_to_leaf_coord(_-[], []):- !.
tree_to_leaf_coord(_-SubTs, [I|Is]):-
  nth0(I, SubTs, SubT),
  tree_to_leaf_coord(SubT, Is).
