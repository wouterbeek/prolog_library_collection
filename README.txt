---+ Prolog generics collection

This repository also contains the PGC.

The generics collection for SWI-Prolog as maintained and used by me in various software projects.

---+ Subcollections

This repository consists of the following subcollections:
* Datasets
* Generics
** Graph theory
** Math
** RDF
** RDFS
** SPARQL
* Standards
** DTD
* Vocabularies

---+ Git commands to include this repository in others

==
git remote add generics https://github.com/wouterbeek/PrologGenerics.git
git fetch generics
git merge generics/master
==

---+ Generic code

Predicates that can be reasonable reused in many contexts. This includes
extensions to SWI-Prolog, or to one of the standard Prolog libraries.

==
  assert(user:file_search_path(datasets,      generic('Datasets'))),
  assert(user:file_search_path(graph_theory,  generic('Graph Theory'))),
  assert(user:file_search_path(math,          generic('Math'))),
  assert(user:file_search_path(owl,           generic('OWL'))),
  assert(user:file_search_path(rdf,           generic('RDF'))),
  assert(user:file_search_path(rdfs,          generic('RDFS'))),
  assert(user:file_search_path(standards,     generic('Standards'))),
  assert(user:file_search_path(dtd,           standards('DTD'))),
  assert(user:file_search_path(vocabularies,  generic('Vocabularies'))),
==
