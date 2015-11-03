:- module(fca_test, [fca_test/0]).

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(dcg/dcg_collection)).
:- use_module(library(dcg/dcg_phrase)).
:- use_module(library(fca/fca2)).
:- use_module(library(graph/build_export_graph)).
:- use_module(library(gv/gv_file)).
:- use_module(library(os/pdf)).

fca_test:-
  fca_test(planets, Context),
  fca_hasse(Context, Hasse),
  build_export_graph(Hasse, ExportG, [vertex_label(concept_label)]),
  gv_export(ExportG, File, []),
  open_pdf(File).

fca_test(planets, context(Os,As,fca_test:planet_property)):-
  aggregate_all(set(O), planet_property(O, _), Os),
  aggregate_all(set(A), planet_property(_, A), As).

planet_property('Mercury', size(small)).
planet_property('Mercury', distance_from_sun(near)).
planet_property('Mercury', moon(no)).
planet_property('Venus',   size(small)).
planet_property('Venus',   distance_from_sun(near)).
planet_property('Venus',   moon(no)).
planet_property('Earth',   size(small)).
planet_property('Earth',   distance_from_sun(near)).
planet_property('Earth',   moon(yes)).
planet_property('Mars',    size(small)).
planet_property('Mars',    distance_from_sun(near)).
planet_property('Mars',    moon(yes)).
planet_property('Jupiter', size(large)).
planet_property('Jupiter', distance_from_sun(far)).
planet_property('Jupiter', moon(yes)).
planet_property('Saturn',  size(large)).
planet_property('Saturn',  distance_from_sun(far)).
planet_property('Saturn',  moon(yes)).
planet_property('Uranus',  size(medium)).
planet_property('Uranus',  distance_from_sun(far)).
planet_property('Uranus',  moon(yes)).
planet_property('Neptune', size(medium)).
planet_property('Neptune', distance_from_sun(far)).
planet_property('Neptune', moon(yes)).
planet_property('Pluto',   size(small)).
planet_property('Pluto',   distance_from_sun(far)).
planet_property('Pluto',   moon(yes)).

concept_label(Concept, Lbl):- string_phrase(dcg_concept(Concept), Lbl).

dcg_concept(concept(Os,As)) --> pair(set(Os), set(As)).
