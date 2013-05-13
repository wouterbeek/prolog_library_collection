:- module(rfc_1737, []).

/** <module> RFC 1737

The specification of a minimal set of requirements for URNs.

# URC, URL, URN

  * *|URC, Uniform Resource Characteristics|*
    Include _|meta-information|_ of a resource.
    Exampels: owner, encoding, access restrictions, cost.
  * *|URL, Uniform Resource Location|*
    Used for _finding_ and _locating_ resources.
    It identifies the location or a container for an instance of a resource
    identified by a URN.
  * *|URN, Uniform Resource Name|*
    Used for _identification_ of a resource or unit of information.

# On the definition of _'resource'_

"In order to build applications in the most general case, the user must be
able to discover and identify the information, objects, or what we will call
in this architecture resources, on which the application is to operate.
Beyond this statement, the URI architecture does not define 'resource.'"

# URN

"The purpose or function of a URN is to provide a globally unique,
persistent identifier used for recognition, for access to
characteristics of the resource or for access to the resource itself."

# Requirements for URN

## Requirements for URN fucntionality

  * *|Global scope|*
    A URN identifies the same resoruce everywhere.
  * *|Global uniqueness|*
    One URN can be assigned to at most one resource.
  * *Persistence*
    The lifetime of a URN is permanent, beyond the lifetime of the resource
    or naming authority.
  * *Scalability*
    URNs can be assigned to any resource.
  * *|Legacy support|*
    Existing naming schemes that satisfy all other requirements should be
    permitted as URNs.
    Examples: ISBN, ISO public identifiers, UPC product codes.
  * *Extensibility*
    Future extensions to the scheme must be possible.
  * *Independence*
    The responsibility for determining the conditions under which a name is
    assigned is local to a name issuing authority.
  * *Resolution*
    If a URN has a corresponding URL, then there should be a feasible
    mechanism for translating from URN to URL.

## Requirements for URN encoding

  * *|Single encoding|*
    The encoding for human presentation is the same as the encoding for
    other transmissions.
  * *|Simple comparison|*
    For every URN, there is a single comparison algorithm.
    This algorithm must be simple, local, and deterministic.
  * *|Human transcribability|*
    URNs should be short, should use a minimum of special characters,
    should be case insensitive.
  * *|Transport friendliness|*
    URNs can be transported unmodified in common Internet protocols.
    Examples: TCP, SMTP, FTP, Telnet, printed paper.
  * *|Machine consumption|*
    URNs can be parsed by a computer.
  * *|Text recognition|*
    URNs should be easy to find and parse when occurring in free text.

@author Wouter Beek
@compar RFC 1737
@see http://www.ietf.org/rfc/rfc1737.txt
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
  rdf_global_id(rfc:'1737', This),
  rdf_assert_datatype(This, rfc:year, gYear, 1994, Graph),
  rdf_assert_literal(This, rfc:title, en, 'Functional Requirements for Uniform Resource Names', Graph),
  rdf_assert_literal(This, rfc:author, en, 'K. Sollins', Graph),
  rdf_assert_literal(This, rfc:author, en, 'L. Masinter', Graph),
  rdf_assert(This, foaf:homepage, 'http://www.ietf.org/rfc/rfc1737.txt', Graph).
:- init.

