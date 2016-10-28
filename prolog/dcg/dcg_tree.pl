:- module(
  dcg_tree,
  [
    dcg_tree//1, % +Tree
    dcg_tree//2  % +Tree, :Opts
  ]
).

/** <module> DCG Trees

@author Wouter Beek
@version 2016/01-2016/02, 2016/05, 2016/07, 2016/10
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(list_ext)).
:- use_module(library(option)).

is_meta(label_writer).
is_meta(node_writer).

:- meta_predicate
    dcg_tree(+, :, ?, ?),
    dcg_tree0(+, +, +, 3, 4, ?, ?),
    dcg_trees0(+, +, 3, 4, ?, ?),
    default_node_writer(3, +, +, ?, ?).





%! dcg_tree(+Tree)// is det.
%! dcg_tree(+Tree, +Opts)// is det.
%
% The following options are supported:
%
%   * label_writer(+callable)
%
%   * node_writer(+callable)

dcg_tree(Tree) -->
  dcg_tree(Tree, []).


dcg_tree(Tree, Opts0) -->
  {
    meta_options(is_meta, Opts0, Opts),
    option(label_writer(Lbl_1), Opts, dcg:dcg_hook),
    option(node_writer(Node_2), Opts, default_node_writer(Lbl_1))
  },
  dcg_trees0([Tree], [], Lbl_1, Node_2).


%! dcg_tree0(+Tree, +InvPath, +IsLast, :Lbl_1, :Node_2)// is det.

dcg_tree0(t(Node,Trees), InvPath, IsLast, Lbl_1, Node_2) -->
  % Determine whether we are about to print a leaf node or not.
  {(Trees = [] -> IsLeaf = true ; IsLeaf = false)},
  dcg_call(Node_2, [Node-IsLast|InvPath], IsLeaf),
  (   {IsLeaf == true}
  ->  ""
  ;   dcg_trees0(Trees, [Node-IsLast|InvPath], Lbl_1, Node_2)
  ).


%! dcg_trees0(+Trees, +InvPath, :Lbl_1, :Node_2)// is det.

dcg_trees0([H|T], InvPath, Lbl_1, Node_2) -->
  % About to print the last node in this list.
  {(T == [] -> IsLast = true ; IsLast = false)},
  dcg_tree0(H, InvPath, IsLast, Lbl_1, Node_2),
  ({IsLast == true} -> "" ; dcg_trees0(T, InvPath, Lbl_1, Node_2)).


default_node_writer(Lbl_1, InvPath, _) -->
  dcg_tree_indent(InvPath),
  {first(InvPath, Node-_)},
  dcg_call(Lbl_1, Node), nl.


dcg_tree_indent([_]) --> !, "".
dcg_tree_indent([Node-IsLast|InvPath]) -->
  {reverse([Node-IsLast|InvPath], Path)},
  tree_pre_indent(Path),
  ({IsLast == true} -> "└" ; "├"),
  " ".


tree_pre_indent([_])  --> !, "".
tree_pre_indent([_-IsLast|Path]) -->
  ({IsLast == true} -> " " ; "│"),
  " ",
  tree_pre_indent(Path).
