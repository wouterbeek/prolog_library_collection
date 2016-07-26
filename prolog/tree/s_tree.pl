:- module(
  s_tree,
  [
    pairs_to_root/2,          % +Pairs, -Root
    pairs_to_tree/2,          % +Pairs, -Tree
    is_s_tree/1,              % @Term
    paths_to_trees/2,         % +AllSubpaths:list(list), -Tree
    print_tree/1,             % +Tree
    print_tree/2,             % +Tree, :Opts
    some_subpaths_to_trees/2, % +SomeSubpaths:list(list), -Tree
    tree_to_graph/2,          % +Tree, -Graph
    tree_depth/2,             % +Tree, -Depth
    tree_to_leaf_coord/2      % +Tree, -Coord:list(nonneg)
  ]
).

/** <module> Simple tree (unlabeled)

Tree datastructure.

### Tree representation

Trees are represented in the following way:

  - Leaf nodes are presented as `t(Node, [])` for unlabeled or as
    `t(Node, _, [])` for labeled trees.

  - Non-leaf nodes are represented as `t(Node, SubTrees)` for
    unlabeled or as `t(Node, Label, SubTrees)` for labeled trees.

A simple example:

```prolog
t(a, [t(b, []), t(c, [])])
```

The above corresponds to the following hierarchical tree
visualization.

```
a
|-b
|-c
```

@author Wouter Beek
@version 2016/01-2016/02, 2016/07
*/

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_tree)).
:- use_module(library(graph/s/s_edge)).
:- use_module(library(list_ext)).
:- use_module(library(option)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).

:- meta_predicate
    print_tree(+, :).

is_meta(node_writer).





%! pairs_to_root(+Pairs, -Root) is semidet.

pairs_to_root(Pairs, Root):-
  member(Root-_, Pairs),
  \+ member(_-Root, Pairs),
  % The root is unique.
  \+ (
    member(SecondRoot-_, Pairs),
    \+ member(_-SecondRoot, Pairs),
    SecondRoot \== Root
  ), !.



%! pairs_to_tree(+Pairs, -Tree) is semidet.
%
% Fails when there are no pairs.

pairs_to_tree(Pairs, Tree):-
  pairs_to_root(Pairs, Root),
  pairs_to_tree(Root, Tree, Pairs, []).


pairs_to_tree(Parent, t(Parent,SubTrees), Pairs1, Pairs3):-
  pairs_to_children(Parent, Children, Pairs1, Pairs2),
  foldl(pairs_to_tree, Children, SubTrees, Pairs2, Pairs3).


pairs_to_children(Parent, [H|T], Pairs1, Pairs3):-
  selectchk(Parent-H, Pairs1, Pairs2), !,
  pairs_to_children(Parent, T, Pairs2, Pairs3).
pairs_to_children(_, [], Pairs, Pairs).



%! is_s_tree(@Term) is semidet.

is_s_tree(t(_,L)) :-
  forall(member(T, L), is_s_tree(T)).



%! paths_to_trees(+Subpaths:list(list), -Tree:compound) is det.
% Creates the tree based on the given subpaths.
%
% ### Examples
%
% ```prolog
% ?- paths_to_trees([[a,b],[a,c]], T).
% T = [t(a, [t(b, []), t(c, [])])].
%
% ?- paths_to_trees([[a,b],[a,c],[a,d]], T).
% T = [t(a, [t(b,[]), t(c, []), t(d, [])])].
%
% ?- paths_to_trees([[a,b],[a,c,e],[a,d]], T).
% T = [t(a, [t(b, []), t(c, [t(e, [])]), t(d, [])])].
%
% ?- paths_to_trees([[a,b],[a,c,e],[a,c,f],[a,d]], T).
% T = [t(a, [t(b, []), t(c, [t(e, []), t(f, [])]), t(d, [])])].
% ```

% Only the empty tree has zero subpaths.
paths_to_trees([], []):- !.
% The subpaths may have different lengths.
% If one subpath is fully processed, we still need to process the rest.
paths_to_trees([[]|Ls], Tree):- !,
  paths_to_trees(Ls, Tree).
% Create the (sub)trees for the given subpaths recursively.
paths_to_trees(Ls1, Trees):-
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
      paths_to_trees(Ls2, Subtrees)
    ),
    Trees
  ).



%! print_tree(+Tree) is det.
%! print_tree(+Tree, +Opts) is det.

print_tree(Tree) :-
  print_tree(Tree, []).


print_tree(Tree, Opts1) :-
  meta_options(is_meta, Opts1, Opts2),
  option(output(Sink), Opts2, current_output),
  dcg_with_output_to(Sink, dcg_tree(Tree, Opts2)).



some_subpaths_to_trees(SomeSubpaths, Trees):-
  aggregate_all(
    set(Subpath),
    (
      member(SomeSubpath, SomeSubpaths),
      sublist(Subpath, SomeSubpath),
      Subpath \== SomeSubpath
    ),
    AllSubpaths
  ),
  paths_to_trees(AllSubpaths, Trees).



%! tree_to_graph(+Tree, -Graph) is det.

tree_to_graph(Tree, Graph) :-
  tree_to_pairs(Tree, Pairs),
  pairs_to_set(Pairs, Vs),
  maplist(pair_edge, Pairs, Es),
  Graph = graph(Vs, Es).



%! tree_to_pairs(+Tree, -Pairs) is det.

tree_to_pairs(t(Root,Trees), Pairs) :-
  findall(
    [Root-Y|Pairs],
    (
      member(t(Y,Subtrees), Trees),
      tree_to_pairs(t(Y,Subtrees), Pairs)
    ),
    Pairss
  ),
  append(Pairss, Pairs).



%! tree_depth(+Tree, -Depth) is det.

tree_depth(_-[], 0):- !.
tree_depth(_-Subtrees, Depth):-
  maplist(tree_depth, Subtrees, Depths),
  max_list(Depths, MaxDepth),
  Depth is MaxDepth + 1.



%! tree_to_leaf_coord(+Tree, -Coord:list(nonneg)) is nondet.

tree_to_leaf_coord(_-[], []):- !.
tree_to_leaf_coord(_-SubTs, [I|Is]):-
  nth0(I, SubTs, SubT),
  tree_to_leaf_coord(SubT, Is).
