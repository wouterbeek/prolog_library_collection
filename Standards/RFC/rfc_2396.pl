:- module(rfc_2396, []).

/** <module> URI

A grammar ad a description of basic functionality for URI.

"This document defines a grammar that is a superset of all valid URI,
such that an implementation can parse the common components of a URI
reference without knowing the scheme-specific requirements of every
possible identifier type.  This document does not define a generative
grammar for URI; that task will be performed by the individual
specifications of each URI scheme."

---+ Abbreviations

    * Uniform Resource Identifier (URI)
      A means for identifying a resource.
    * Resource
      ???

@author Wouter Beek
@compat RFC 2396
@see http://www.ietf.org/rfc/rfc2396.txt
@tbd
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(foaf, 'http://xmlns.com/foaf/0.1/').
:- xml_register_namespace(rfc, 'http://www.ietf.org/rfc/').



init:-
  Graph = rfc,
  rdf_global_id(rfc:'2396', This),
  rdf_assert_datatype(This, rfc:year, gYear, 1998, Graph),
  rdf_assert_literal(This, rfc:title, en, 'Uniform Resource Identifiers (URI): Generic Syntax', Graph),
  rdf_assert_literal(This, rfc:author, en, 'Tim Berners-Lee', Graph),
  rdf_assert_literal(This, rfc:author, en, 'R. Fielding', Graph),
  rdf_assert_literal(This, rfc:author, en, 'U.C. Irvine', Graph),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', Graph),
  rdf_assert(This, foaf:homepage, 'http://www.ietf.org/rfc/rfc2396.txt', Graph),
  rdf_assert(This, rfc:mentions, rfc:'1630', Graph),
  rdf_assert(This, rfc:implements, rfc:'1736', Graph),
  rdf_assert(This, rfc:implements, rfc:'1737', Graph),
  rdf_assert(This, rfc:updates, rfc:'1738', Graph),
  rdf_assert(This, rfc:updates, rfc:'1808', Graph).
:- init.

