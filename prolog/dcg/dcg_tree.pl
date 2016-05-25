:- module(
  dcg_tree,
  [
    dcg_tree//1, % +Tree
    dcg_tree//2  % +Tree, :Opts
  ]
).

/** <module> DCG Trees

@author Wouter Beek
@version 2016/01-2016/02, 2016/05
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(list_ext)).
:- use_module(library(option)).

is_meta(node_writer).

:- meta_predicate
    dcg_tree(+, :, ?, ?).





%! dcg_tree(+Tree)// is det.
%! dcg_tree(+Tree, +Opts)// is det.
%
% The following options are supported:
%   * node_writer(+callable)

dcg_tree(Tree) -->
  dcg_tree(Tree, []).


dcg_tree(Tree, Opts1) -->
  {meta_options(is_meta, Opts1, Opts2)},
  dcg_trees0([Tree], [], Opts2).


%! dcg_tree0(+Tree, +InvPath, +IsLast, +Opts)// is det.

dcg_tree0(t(Node,Trees), InvPath, IsLast, Opts) -->
  {(Trees = [] -> IsLeaf = true ; IsLeaf = false)},
  {option(node_writer(NodeWriter_4), Opts, default_node_writer)},
  dcg_call(NodeWriter_4, [Node-IsLast|InvPath], IsLeaf),
  ({IsLeaf == true} -> "" ; dcg_trees0(Trees, [Node-IsLast|InvPath], Opts)).


%! dcg_trees0(+Trees, +InvPath, +Opts)// is det.

dcg_trees0([H|T], InvPath, Opts) -->
  {(T == [] -> IsLast = true ; IsLast = false)},
  dcg_tree0(H, InvPath, IsLast, Opts),
  ({IsLast == true} ->  "" ; dcg_trees0(T, InvPath, Opts)).


default_node_writer(InvPath, _) -->
  dcg_tree_indent(InvPath),
  {first(InvPath, Node-_)},
  atom(Node), nl.


dcg_tree_indent([_]) --> !, "".
dcg_tree_indent([_-IsLast|InvPath]) -->
  tree_pre_indent(InvPath),
  ({IsLast == true} -> "└" ; "├"),
 " ".


tree_pre_indent([_])  --> !, "".
tree_pre_indent([_-IsLast|InvPath]) -->
  ({IsLast == true} -> " " ; "│"),
  " ",
  tree_pre_indent(InvPath).
