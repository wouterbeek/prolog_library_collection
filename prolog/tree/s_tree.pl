:- module(
  s_tree,
  [
    edges_to_root/2,          % +Es, -Root
    edges_to_tree/2,          % +Es, -Tree
    is_s_tree/1,              % @Term
    paths_to_trees/2,         % +AllSubPaths:list(list), -Tree
    print_tree/1,             % +Tree
    print_tree/2,             % +Tree, :Opts
    some_subpaths_to_trees/2, % +SomeSubPaths:list(list), -Tree
    tree_to_graph/2,          % +Tree, -Graph
    tree_depth/2,             % +Tree, -Depth
    tree_to_leaf_coord/2      % +Tree, -Coord:list(nonneg)
  ]
).

/** <module> Simple tree (unlabeled)

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
@version 2016/01-2016/02
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

:- predicate_options(print_tree/2, 2, [
     output(+stream),
     pass_to(dcg_tree//2, 2)
   ]).





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



some_subpaths_to_trees(SomeSubPaths, Trees):-
  aggregate_all(
    set(SubPath),
    (
      member(SomeSubPath, SomeSubPaths),
      sublist(SubPath, SomeSubPath),
      SubPath \== SomeSubPath
    ),
    AllSubPaths
  ),
  paths_to_trees(AllSubPaths, Trees).



%! tree_to_graph(+Tree, -Graph) is det.

tree_to_graph(Tree, Graph) :-
  tree_to_edges(Tree, Es),
  s_edges_vertices(Es, Vs),
  Graph = graph(Vs, Es).

tree_to_edges(t(X,Ts), Es) :-
  findall([X-Y|Es], (member(t(Y,Ts0), Ts), tree_to_edges(t(Y,Ts0), Es)), Ess),
  append(Ess, Es).



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
