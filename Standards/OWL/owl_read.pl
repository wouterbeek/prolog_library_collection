:- module(
  owl_read,
  [
    owl_class_equivalence/2, % ?Class1:uri
                             % ?Class2:uri
    owl_resource_identity/2 % ?Resource1:uri
                            % ?Resource2:uri
  ]
).

/** <module> OWL read

Predicates for reading from OWL data.

@author Wouter Beek
@version 2013/01, 2013/03
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_namespace)).

:- rdf_meta(owl_class_equivalence(r,r)).
:- rdf_meta(owl_resource_identity(r,r)).

:- rdf_register_namespace(owl).



owl_class_equivalence(Class1, Class2):-
  rdf_has(Class1, owl:equivalentClass, Class2).

owl_resource_identity(Resource1, Resource2):-
  rdf_has(Resource1, owl:sameAs, Resource2).

