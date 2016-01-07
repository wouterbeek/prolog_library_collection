:- module(
  dcg_tree,
  [
    dcg_tree//1, % +Tree
    dcg_tree//2, % :Node_3, +Tree
    dcg_tree//3  % :Node_3, +Tree, +Opts
  ]
).

/** <module> DCG Trees

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(dcg/dcg_call)).
:- use_module(library(dcg/dcg_content)).
:- use_module(library(option)).

:- meta_predicate
    dcg_node0(+, 3, +, ?, ?),
    dcg_tree(3, +, ?, ?),
    dcg_tree(3, +, +, ?, ?),
    dcg_tree0(+, 3, +, ?, ?),
    dcg_trees0(+, 3, +, ?, ?).





%! dcg_tree(+Tree)// is det.
%! dcg_tree(:Node_3, +Tree)// is det.
%! dcg_tree(:Node_3, +Tree, +Opts)// is det.
% The following options are supported:
%   * indent(+nonneg)

dcg_tree(Tree) -->
  dcg_tree(atom, Tree).
dcg_tree(Node_3, Tree) -->
  dcg_tree(Node_3, Tree, []).
dcg_tree(Node_3, Tree, Opts) -->
  {option(indent(I), Opts, 0)},
  dcg_tree0(I, Node_3, Tree).


%! dcg_tree0(+Indent:nonneg, :Node_3, +Tree)// is det.

dcg_tree0(I1, Node_3, Node-Trees) -->
  dcg_node0(I1, Node_3, Node),
  (   {Trees == []}
  ->  ""
  ;   {I2 is I1 + 1},
      dcg_trees0(I2, Node_3, Trees)
  ).


%! dcg_trees0(+Indent:nonneg, :Node_3, +Trees:list)// is det.

dcg_trees0(I, Node_3, [H|T]) -->
  dcg_tree0(I, Node_3, H),
  (   {T == []}
  ->  ""
  ;   dcg_trees0(I, Node_3, T)
  ).


%! dcg_node0(+Indent:nonneg, :Node_3, +Node)// is det.

dcg_node0(I1, Node_3, Node) -->
  (   {I1 =:= 0}
  ->  ""
  ;   {I2 is I1 - 1},
      tab(I2),
      "|-"
  ),
  dcg_once(dcg_call(Node_3, Node)),
  nl.
