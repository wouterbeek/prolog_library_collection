:- module(
  skos_build,
  [
    skos_assert_broader/3, % +Broader:uri
                           % +Narrower:uri
                           % +Graph:atom
    skos_assert_narrower/3, % +Narrower:uri
                            % +Broader:uri
                            % +Graph:atom
    skos_assert_hierarchy/3 % +Tree:tree
                            % +Scheme:atom
                            % +Graph:atom
  ]
).

/** <module> SKOS BUILD

@author Wouter Beek
@version 2013/04-2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(skos, 'http://www.w3.org/2004/02/skos/core#').

:- rdf_meta(skos_assert_broader(r,r,+)).
:- rdf_meta(skos_assert_hierarchy(+,r,+)).
:- rdf_meta(skos_assert_narrower(r,r,+)).
:- rdf_meta(skos_assert_tree(+,r,+)).



skos_assert_broader(Broader, Narrower, Graph):-
  rdf_assert(Broader, skos:broader, Narrower, Graph).

skos_assert_hierarchy(Root-Branches, Scheme, Graph):-
  rdfs_assert_individual(Scheme, skos:'ConceptScheme', Graph),
  rdf_assert(Scheme, skos:hasTopConcept, Root, Graph),
  rdfs_assert_individual(Root, skos:'Concept', Graph),
  skos_assert_tree(Root-Branches, Scheme, Graph).

skos_assert_narrower(Narrower, Broader, Graph):-
  rdf_assert(Narrower, skos:narrower, Broader, Graph).

skos_assert_tree(Root-Branches, Scheme, Graph):-
  rdf_assert(Root, skos:inSchema, Scheme, Graph),
  forall(
    member(Tree, Branches),
    (
      Tree = NewRoot-_NewBranches,
      skos_assert_broader(Root, NewRoot, Graph),
      skos_assert_tree(Tree, Scheme, Graph)
    )
  ).

