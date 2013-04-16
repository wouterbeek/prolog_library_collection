:- module(
  owl_build,
  [
    owl_assert_class_equivalence/3, % +Class1:class
                                    % +Class2:class
                                    % +Graph:atom
    owl_assert_resource_identity/3, % +Resource1:class
                                    % +Resource2:class
                                    % +Graph:atom
    owl_retractall_class_equivalence/3, % +Class1:class
                                        % +Class2:class
                                        % +Graph:atom
    owl_retractall_resource_identity/3 % +Resource1:class
                                       % +Resource2:class
                                       % +Graph:atom
  ]
).

/** <module> OWL build

Predicates for building OWL ontologies.

@author Wouter Beek
@version 2012/12-2013/01, 2013/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_namespace)).

:- rdf_meta(owl_assert_class_equivalence(r,r,+)).
:- rdf_meta(owl_assert_resource_identity(r,r,+)).
:- rdf_meta(owl_retractall_class_equivalence(r,r,+)).
:- rdf_meta(owl_retractall_resource_identity(r,r,+)).

:- rdf_register_namespace(owl).



owl_assert_class_equivalence(Class1, Class2, Graph):-
  rdf_assert(Class1, owl:equivalentClass, Class2, Graph).

owl_assert_resource_identity(Resource1, Resource2, Graph):-
  rdf_assert(Resource1, owl:sameAs, Resource2, Graph).

owl_retractall_class_equivalence(Class1, Class2, Graph):-
  rdf_retractall(Class1, owl:equivalentClass, Class2, Graph).

owl_retractall_resource_identity(Resource1, Resource2, Graph):-
  rdf_retractall(Resource1, owl:sameAs, Resource2, Graph).

