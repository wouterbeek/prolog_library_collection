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

Predicates that can be reused in many contexts. This includes
extensions to SWI-Prolog, or to one of the standard Prolog libraries.
