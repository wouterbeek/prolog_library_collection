:- module(
  rdf_statistics,
  [
    count_classes/2, % +Graph:atom
                     % -Count:integer
    count_individuals/3, % +Class:uri
                         % +Graph:atom
                         % -Count:integer
    count_objects/4, % +Subject:oneof([bnode,uri])
                     % +Predicate:uri
                     % +Graph:atom
                     % -Count:integer
    count_predicates/4, % +Subject:oneof([bnode,uri])
                        % +Object:resource
                        % +Graph:atom
                        % -Count:integer
    count_properties/2, % +Graph:atom
                        % -Count:integer
    count_subjects/3, % +PO_Pairs:list(pair)
                      % +Graphs:oneof([atom,list(atom)])
                      % -Count:integer
    count_subjects/4 % +Predicate:uri
                     % +Object:resource
                     % +Graph:oneof([atom,list(atom)])
                     % -Count:integer
  ]
).

/** <module> RDF statistics

Statistics for RDF data.

@author Wouter Beek
@version 2013/01, 2013/03-2013/04
*/

:- use_module(generics(meta_ext)).
:- use_module(library(semweb/rdf_db)).
:- use_module(library(semweb/rdfs)).
:- use_module(rdf(rdf_graph)).

:- rdf_meta(count_classes(+,-)).
:- rdf_meta(count_individuals(r,+,-)).
:- rdf_meta(count_objects(r,r,+,-)).
:- rdf_meta(count_predicates(r,r,+,-)).
:- rdf_meta(count_properties(+,-)).
:- rdf_meta(count_subjects(r,r,+,-)).



count_classes(Graph, Count):-
  setoff(
    Class,
    rdf(_Individual, rdf:type, Class, Graph),
    Classes
  ),
  length(Classes, Count).

count_individuals(Class, Graph, Count):-
  setoff(
    Individual,
    (
      rdfs_individual_of(Individual, Class),
      rdf_subject(Graph, Individual)
    ),
    Individuals
  ),
  length(Individuals, Count).

count_objects(Subject, Predicate, Graph, Count):-
  setoff(
    Object,
    rdf(Subject, Predicate, Object, Graph),
    Objects
  ),
  length(Objects, Count).

count_predicates(Subject, Object, Graph, Count):-
  setoff(
    Predicate,
    rdf(Subject, Predicate, Object, Graph),
    Predicates
  ),
  length(Predicates, Count).

count_properties(Graph, Count):-
  setoff(
    Property,
    rdf(_Subject, Property, _Object, Graph),
    Properties
  ),
  length(Properties, Count).

count_subjects(PO_Pairs, Gs, Count):-
  PO_Pairs = [P1-O1 | Other_PO_Pairs],
  setoff(
    X,
    (
      member(G1, Gs),
      rdf(X, P1, O1, G1),
      forall(
        member(P-O, Other_PO_Pairs),
        (
          member(G, Gs),
          rdf(X, P, O, G)
        )
      )
    ),
    Xs
  ),
  length(Xs, Count).

count_subjects(Predicate, Object, Graph, Count):-
  atom(Graph),
  rdf_graph(Graph),
  !,
  count_subjects0(Predicate, Object, Graph, Count).
count_subjects(Predicate, Object, Graphs, Count):-
  is_list(Graphs),
  maplist(rdf_graph, Graphs),
  mapsum(count_subjects0(Predicate, Object), Graphs, Count).

count_subjects0(Predicate, Object, Graph, Count):-
  setoff(
    Subject,
    rdf(Subject, Predicate, Object, Graph),
    Subjects
  ),
  length(Subjects, Count).

