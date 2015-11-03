:- module(
  fca2,
  [
    fca_hasse/2 % +Context:compound
                % -Hasse:ugraph
  ]
).

:- use_module(library(aggregate)).
:- use_module(library(apply)).
:- use_module(library(graph/s/s_graph)).
:- use_module(library(list_ext)).
:- use_module(library(ordsets)).



fca_hasse(Context, Hasse):-
  context_components(Context, _, As, _),
  maplist(singleton, Ass0, As),
  maplist(as2os(Context), Ass0, Oss0),
  intersections(Oss0, Oss),
  maplist(os2as(Context), Oss, Ass),
  maplist(concept_components, Cs, Oss, Ass),
  aggregate_all(
    set(C1-C2),
    (
      member(C1, C2, Cs),
      maplist(concept_components, [C1,C2], [OsC1,OsC2], _),
      ord_subset(OsC1, OsC2)
    ),
    Es
  ),
  s_graph_components(Hasse, Cs, Es).


intersections(Ss1, Sol):-
  intersections(Ss1, Ss1, Ss1, Sol).

intersections(Ss1, Ss2, Hist1, Sol):-
  aggregate_all(
    set(S3),
    (
      member(S1, Ss1),
      member(S2, Ss2),
      ord_intersection(S1, S2, S3),
      \+ member(S3, Hist1)
    ),
    Ss3
  ),
  (   Ss3 == []
  ->  Sol = Hist1
  ;   ord_union(Hist1, Ss3, Hist2),
      intersections(Ss1, Ss3, Hist2, Sol)
  ).


as2os(context(Os,_,_), [], Os):- !.
as2os(context(_,_,Goal_2), [A1|As], Os):-
  aggregate_all(
    set(O),
    (call(Goal_2, O, A1), forall(member(A2, As), call(Goal_2, O, A2))),
    Os
  ).


os2as(context(_,As,_), [], As):- !.
os2as(context(_,_,Goal_2), [O1|Os], As):-
  aggregate_all(
    set(A),
    (call(Goal_2, O1, A), forall(member(O2, Os), call(Goal_2, O2, A))),
    As
  ).


singleton([X], X).


context_components(context(Os,As,Goal_2), Os, As, Goal_2).


concept_components(concept(Os,As), Os, As).
