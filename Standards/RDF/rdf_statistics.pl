:- module(
  rdf_statistics,
  [
    count_classes/2, % +Graph:atom
                     % -Count:integer
    count_individuals/3, % +Class:uri
                         % +Graph:atom
                         % -Count:integer
    count_objects/4, % +Subject:uri
                     % +Predicate:uri
                     % +Graph:atom
                     % -Count:integer
    count_predicates/4, % +Subject:uri
                        % +Object:resource
                        % +Graph:atom
                        % -Count:integer
    count_properties/2, % +Graph:atom
                        % -Count:integer
    count_subjects/4 % +Predicate:uri
                     % +Object:resource
                     % +Graph:atom
                     % -Count:integer
  ]
).

/** <module> RDF statistics

Statistics for RDF data.

@author Wouter Beek
@version 2013/01, 2013/03
*/

:- use_module(generic(meta_ext)).
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

count_subjects(Predicate, Object, Graph, Count):-
  setoff(
    Subject,
    rdf(Subject, Predicate, Object, Graph),
    Subjects
  ),
  length(Subjects, Count).

