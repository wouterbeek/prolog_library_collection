:- module(
  fca_viz,
  [
    fca_export_graph/2, % +Context, -ExportGraph
    fca_export_graph/3, % +Context, -ExportGraph, :Opts
    fca_viz/2,          % +Context, ?File
    fca_viz/3           % +Context, ?File, :Opts
  ]
).

/** <module> FCA visualization

@author Wouter Beek
@version 2015/11-2016/01
*/

:- use_module(library(aggregate)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_pl)).
:- use_module(library(fca/fca)).
:- use_module(library(graph/build_export_graph)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(gv/gv_file)).
:- use_module(library(option)).
:- use_module(library(ordsets)).

:- meta_predicate(fca_export_graph(+,?,:)).
:- meta_predicate(fca_viz(+,?,:)).

:- predicate_options(fca_export_graph/3, 3, [
     concept_label(+callable)
   ]).
:- predicate_options(fca_viz/3, 3, [
     pass_to(fca_export_graph/3, 3),
     pass_to(graph_viz/3, 3)
   ]).

is_meta(concept_label).





%! fca_export_graph(+Context:compound, -ExportGraph:compound) is det.
% Wrapper around fca_export_graph/3 with default options.

fca_export_graph(Context, ExportG):-
  fca_export_graph(Context, ExportG, []).


%! fca_export_graph(
%!   +Context:compound,
%!   -ExportGraph:compound,
%!   :Options:list(compound)
%! ) is det.
% The following optios are supported:
%   * concept_label(+callable)
%     DCG writing the labels for individual concepts.

fca_export_graph(Context, ExportG, Opts1):-
  fca_hasse(Context, Hasse),
  meta_options(is_meta, Opts1, Opts2),
  option(concept_label(Label_3), Opts2, concept_label),
  merge_options(
    [
      vertex_label(Label_3),
      vertex_rank(fca:concept_cardinality)
    ],
    Opts2,
    Opts3
  ),
  build_export_graph(Hasse, ExportG, Opts3).



%! fca_viz(+Context:compound, ?File:atom) is det.
% Wrapper around fca_viz/3 with default options.

fca_viz(Context, File):-
  fca_viz(Context, File, []).


%! fca_viz(+Context:compound, ?File:atom, :Options:list(compound)) is det.

fca_viz(Context, File, Opts1):-
  meta_options(is_meta, Opts1, Opts2),
  statistics(process_cputime, Time1),
  fca_export_graph(Context, ExportG, Opts2),
  ExportG = graph(_,_,Es,_),
  aggregate_all(max(N), (member(edge(V1,V2,_), Es), (N = V1 ; N = V2)), N),
  ignore(option(number_of_vertices(N), Opts2)),
  statistics(process_cputime, Time2),
  Time is Time2 - Time1,
  ignore(option(process_cputime(Time), Opts2)),
  graph_viz(ExportG, File, Opts2).



%! concept_label(+Concept:compound)// is det.

concept_label(concept(Os,As)) --> set(Os), " / ", set(As).
