:- module(
  dcg_tree,
  [
    dcg_tree//1, % +Tree
    dcg_tree//2, % :Node_5, +Tree
    dcg_tree//3  % :Node_5, +Tree, +Opts
  ]
).

/** <module> DCG Trees

@author Wouter Beek
@version 2016/01-2016/02
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(option)).

:- meta_predicate
    dcg_node0(+, 5, +, +, +, ?, ?),
    dcg_tree(5, +, ?, ?),
    dcg_tree(5, +, +, ?, ?),
    dcg_tree0(+, 5, +, +, ?, ?),
    dcg_trees0(+, 5, +, +, ?, ?).





%! dcg_tree(+Tree)// is det.
%! dcg_tree(:Node_5, +Tree)// is det.
%! dcg_tree(:Node_5, +Tree, +Opts)// is det.
% The following options are supported:
%   * indent(+nonneg)

dcg_tree(Tree) -->
  dcg_tree(default_node, Tree).

dcg_tree(Node_5, Tree) -->
  dcg_tree(Node_5, Tree, []).

dcg_tree(Node_5, Tree, Opts) -->
  {option(indent(I), Opts, 0)},
  dcg_tree0(I, Node_5, Tree, []).

default_node(Node, _, _) --> atom(Node).


%! dcg_tree0(+Indent:nonneg, :Node_5, +Tree, +InvPath)// is det.

dcg_tree0(I1, Node_5, t(Node,Trees), InvPath) -->
  {(Trees = [] -> Leaf = true ; Leaf = false)},
  dcg_node0(I1, Node_5, Node, InvPath, Leaf),
  (   {Leaf == true}
  ->  ""
  ;   {I2 is I1 + 1},
      dcg_trees0(I2, Node_5, Trees, [Node|InvPath])
  ).


%! dcg_trees0(+Indent:nonneg, :Node_5, +Trees:list, +InvPath)// is det.

dcg_trees0(I, Node_5, [H|T], InvPath) -->
  dcg_tree0(I, Node_5, H, InvPath),
  (   {T == []}
  ->  ""
  ;   dcg_trees0(I, Node_5, T, InvPath)
  ).


%! dcg_node0(+Indent:nonneg, :Node_5, +Node, +InvPath, +Leaf)// is det.

dcg_node0(I1, Node_5, Node, InvPath, Leaf) -->
  (   {I1 =:= 0}
  ->  ""
  ;   {I2 is I1 - 1},
      tabby(I2),
      "├ "
  ),
  dcg_once(dcg_call(Node_5, Node, InvPath, Leaf)),
  nl.

tabby(0) --> !, "".
tabby(N1) --> "│ ", {N2 is N1 - 1}, tabby(N2).
