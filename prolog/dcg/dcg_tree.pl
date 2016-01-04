:- module(
  dcg_tree,
  [
    dcg_tree/2, % :Node_3, +Tree
    dcg_tree/3  % :Node_3, +Tree, +Opts
  ]
).

/** <module> DCG Trees

@author Wouter Beek
@version 2016/01
*/

:- use_module(library(dcg_call)).
:- use_module(library(dcg_content)).
:- use_module(library(option)).

:- meta_predicate
	dcg_node(+, 3, +),
	dcg_tree(3, +),
	dcg_tree(3, +, +),
	dcg_trees(+, 3, +).





%! dcg_tree(:Node_3, +Tree)// is det.
%! dcg_tree(:Node_3, +Tree, +Opts)// is det.
% The following options are supported:
%   * indent(+nonneg)

dcg_tree(Node_3, Tree) -->
  dcg_tree(Node_3, Tree, []).
dcg_tree(Node_3, Tree, Opts) -->
  {option(indent(I), Opts, 0)},
	dcg_tree(I, Node_3, Tree).


%! dcg_tree(+Indent:nonneg, :Node_3, +Tree)// is det.

dcg_tree(I1, Node_3, Node-Trees) -->
	dcg_node(I1, Node_3, Node),
	(   {Trees == []}
	->  ""
	;   {I2 is I1 + 1},
	    dcg_trees(I2, Node_3, Trees)
	).


%! dcg_trees(+Indent:nonneg, :Node_3, +Trees:list)// is det.

dcg_trees(I, Node_3, [H|T]) -->
	dcg_tree(I, Node_3, H),
	(   {T == []}
	->  ""
	;   dcg_trees(I, Node_3, T)
	).


%! dcg_node(+Indent:nonneg, :Node_3, +Node)// is det.

dcg_node(I1, Node_3, Node) -->
	(   {I1 =:= 0}
	->  ""
	;   {I2 is I1 - 1},
	    dcg_tab(I2),
	    " |-"
	),
	dcg_once(Node_3, Node),
	dcg_nl.
