:- module(rfc_1736, []).

/** <module> RFC 1736

Document specifying a mnimum set of requirements for Internet resource
locators.

# Resource identifiers

  * *|Resource locator|*
    Conveys location and access information for a resource.
  * *|Resource name|*
    A stable handle to refer to a resource long after the resource itself
    has moved or perhaps gone out of existence.
  * *|Resource description|*
    Comprises a body of meta-information to assist resource search and
    selection.

# General Resource Locator

A general resource locator is an object that describes the location of a
resource.

The ability to access the location is not guaranteed:
"Locators may apply to resources that are not always or not ever
network accessible. Examples of the latter include human beings and
physical objects that have no electronic instantiation [...]."

# Resource locator interpreter

"A prerequisite to interpreting a locator is understanding when an
object in question actually is a locator, or contains one or more
locators."

# Possible aspects of resource locator uniqueness

  1. No identical copies of a resource may exist.
  1. The same resource accessed in different attempts is the same. [???]
  1. A resource may have at most one locator.
  1. A resource locator must identity exactly one object.

# Access and availability

  * _Invalid_ resource locator
    A resource locator that conforms to a location standard but for which
    resource access fails consistently.
  * _Valid_ resource locator
    A resource locator that conforms to a location standard and that can
    either access a resource, or that cannot access a resource which is
    unavailable (but does exist). Unavailability categories: (1) The
    resource is available during predicatable intervals of which Now is not
    a member.
    (2) The resource is temporarily unavailable due to heavy system loading.
    (3) The interpretor does not have rights clearance to the resource.

# Requirements

  1. Locators are _transient_ (as opposed to names).
  1. Locators have _|global scope|_.
  1. Locators are _parsable_.
  1. Locators can be readily distinguished from naming and descriptive
     identifiers that occupy the same namespace. [???]
  1. Locators can be _transported_ across Internet standard communication
     protocols without loss or corruption of information.
  1. Locators are _|human transcribable|_.
  1. Locators consist of a _service_ and an opaque _|parameter package|_.
     [???]
  1. The set of _services_ is extensible. [???]
  1. Locators contain no information about the resource other than that
     required by the access mechanism.
  1. Dropped requirement: Software can recognize resource locators in
     unstructured environments (e.g. natural language ASCII text) with a
     high degree of probability.

# Missing definitions

The text mentions the following concepts without explaining them:
  * *|Abstract access method|*
    Examples: a software tool, an instruction, a network protocol.
  * *|Parameter package|*
    Examples: service-specific access instructions.
  * *Service*
    An _|abstract access method|_.

@author Wouter Beek
@compat RFC 1736
@see http://www.ietf.org/rfc/rfc1736.txt
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
  rdf_global_id(rfc:'1736', This),
  rdf_assert_datatype(This, rfc:year, gYear, 1995, Graph),
  rdf_assert_literal(This, rfc:title, en, 'Functional Recommendations for Internet Resource Locators', Graph),
  rdf_assert_literal(This, rfc:author, en, 'J. Kunze', Graph),
  rdf_assert(This, foaf:homepage, 'http://www.ietf.org/rfc/rfc1736.txt', Graph).
:- init.

