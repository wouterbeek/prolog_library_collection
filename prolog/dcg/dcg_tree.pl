:- module(
  dcg_tree,
  [
    dcg_tree//1, % +Tree
    dcg_tree//2  % +Tree, :Opts
  ]
).

/** <module> DCG Trees

@author Wouter Beek
@version 2016/01-2016/02
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(option)).

is_meta(node_writer).

:- meta_predicate
    dcg_tree(+, :, ?, ?).



%! dcg_tree(+Tree)// is det.
%! dcg_tree(+Tree, +Opts)// is det.
% The following options are supported:
%   * node_writer(+callable)

dcg_tree(Tree) -->
  dcg_tree(Tree, []).

dcg_tree(Tree, Opts1) -->
  {meta_options(is_meta, Opts1, Opts2)},
  dcg_tree0(Tree, [], Opts2).


%! dcg_tree0(+Tree, +InvPath, +Opts)// is det.

dcg_tree0(t(Node,Trees), InvPath, Opts) -->
  {(Trees = [] -> Leaf = true ; Leaf = false)},
  {option(node_writer(NodeWriter_4), Opts, default_node_writer)},
  dcg_call(NodeWriter_4, [Node|InvPath], Leaf), nl,
  ({Leaf == true} -> "" ; dcg_trees0(Trees, [Node|InvPath], Opts)).


%! dcg_trees0(+Trees:list, +InvPath, +Opts)// is det.

dcg_trees0([H|T], InvPath, Opts) -->
  dcg_tree0(H, InvPath, Opts),
  ({T == []} ->  "" ; dcg_trees0(T, InvPath, Opts)).


default_node_writer([Node|InvPath], _) -->
  indent_path(InvPath),
  atom(Node), nl.

indent_path(Path) -->
  {length(Path, Len)},
  ({Len =:= 0} -> "" ; {Tabs is Len - 1}, tabby(Tabs), "├ ").

tabby(0)  --> !, "".
tabby(N1) --> "│ ", {N2 is N1 - 1}, tabby(N2).
