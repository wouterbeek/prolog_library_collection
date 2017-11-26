:- module(
  tree_viz,
  [
    tree_export_graph/2, % +Tree, ExportG
    tree_export_graph/3, % +Tree, ExportG, +Opts
    tree_viz/2,          % +Tree, ?File
    tree_viz/3           % +Tree, ?File, +Opts
  ]
).

/** <module> Tree visualization

Export trees to GraphViz.

@author Wouter Beek
@version 2016/01-2016/02
*/

:- use_module(library(graph/build_export_graph)).
:- use_module(library(gv/gv_file)).
:- use_module(library(ordsets)).
:- use_module(library(tree/l_tree)).
:- use_module(library(tree/s_tree)).

:- predicate_options(tree_export_graph/3, 3, [
     pass_to(build_export_graph/4, 4)
   ]).
:- predicate_options(tree_viz/3, 3, [
     pass_to(graph_viz/3, 3),
     pass_to(tree_to_graph/3, 3)
   ]).





%! tree_export_graph(+Tree, -ExportG) is det.
%! tree_export_graph(+Tree, -ExportG, +Opts) is det.
% Opts are passed to build_export_graph/4.

tree_export_graph(Tree, ExportG) :-
  tree_export_graph(Tree, ExportG, []).

tree_export_graph(Tree, ExportG, Opts) :-
  (   is_s_tree(Tree)
  ->  tree_to_graph(Tree, G)
  ;   is_l_tree(Tree)
  ->  l_tree_to_graph(Tree, G)
  ),
  build_export_graph(G, ExportG, Opts).



%! tree_viz(+Tree, ?File) is det.
%! tree_viz(+Tree, ?File, +Opts) is det.
% Stores the given tree term into a GraphViz file.
%
% Options are passed to export_graph_to_gv_file/3, tree_to_graph/3.

tree_viz(Tree, File) :-
  tree_viz(Tree, File, []).

tree_viz(Tree, File, Opts) :-
  tree_export_graph(Tree, ExportG, Opts),
  graph_viz(ExportG, File, Opts).
