:- module(
  owl_read,
  [
    owl_class_equivalence/2, % ?Class1:uri
                             % ?Class2:uri
    owl_resource_identity/2, % ?Resource1:uri
                             % ?Resource2:uri
    owl_resource_identity/4, % ?Resource1:uri
                             % ?Graph1:atom
                             % ?Resource2:uri
                             % ?Graph2:atom
    owl_resource_identity/5 % ?Resource1:uri
                            % ?Graph1:atom
                            % ?Resource2:uri
                            % ?Graph2:atom
                            % ?LinkGraph:atom
  ]
).

/** <module> OWL read

Predicates for reading from OWL data.

@author Wouter Beek
@version 2013/01, 2013/03-2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(xml(xml_namespace)).

:- rdf_meta(owl_class_equivalence(r,r)).
:- rdf_meta(owl_resource_identity(r,r)).

:- xml_register_namespace(owl, 'http://www.w3.org/2002/07/owl#').
:- xml_register_namespace(rdf, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#').



owl_class_equivalence(Class1, Class2):-
  rdf_has(Class1, owl:equivalentClass, Class2).

owl_resource_identity(Resource1, Resource2):-
  rdf_has(Resource1, owl:sameAs, Resource2).

owl_resource_identity(Resource1, Graph1, Resource2, Graph2):-
  owl_resource_identity(Resource1, Graph1, Resource2, Graph2, _LinkGraph).

owl_resource_identity(Resource1, Graph1, Resource2, Graph2, LinkGraph):-
  rdf(Resource1, owl:sameAs, Resource2, LinkGraph),
  rdf(Resource1, rdf:type, _Class1, Graph1),
  rdf(Resource2, rdf:type, _Class2, Graph2).

