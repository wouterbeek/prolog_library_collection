:- module(xml_schema_primer, []).

/** <module> XML SCHEMA PRIMER

  * Elements that contain subelements and/or carry attributes have
    *|complex types|*.
  * Elements that contain values of a specific datatype have *|simple types|*.

---+ Attribute

An attribute can occur at most once for an element.

Default values are given for missing attributes that have optional use
and a set default value.

*|Global attribute|*: an attribute whose declaration is a child of
=xsd:schema=.

---++ Attribute declaration

Entity =xsd:atttribute=.

Attributes;
  * =default=
    Use must be optional. Excludes a fixed value.
  * =fixed=
    A value that does not change. Excludes a default value.
  * =name=
  * =type=
  * =use=
    Allowed values: =optional=, =prohibited=, =required=. Only for non-global
    elements.

---+ Complex types

Complex types are defined using the =xsd:complexType= element,
with attribute =name=.

They can contain element declarations, element references, and
attribute declarations.

---+ Element

Default values are set for missing elements that have optional use
and a set default value and that do occur as empty elements.
(Missing elements do not receive their default value.)

*|Global element|*: an element whose declaration is a child of =xsd:schema=.
Global elements can appear at the top-level of an instance document.

---++ Element declaration

Entity =xsd:element=

Attributes:
  * =default=
    Use must be optional. Excludes a fixed value.
  * =fixed=
    A value that does not change. Excludes a default value.
  * =maxOccurs=
    Default: 1. Only for non-global attributes.
  * =minOccurs=
    Default: 1. Only for non-global attributes.
  * =name=
  * =ref=
    Reference an existing global element.
    This does not apply to global elements (they must use =type=).
  * =type=

---+ Simple type

Built-in simple types:
* anyURI
* base64Binary
* boolean
* byte
** unsignedByte
* date
* dateTime
* decimal
* double
* duration
* ENTITY
* ENTITIES
* float
* gDay
* gMonth
* gMonthDay
* gYear
* gYearMonth
* hexBinary
* ID
* IDREF
* IDREFS
* int
** unsignedInt
* integer
** negativeInteger
** nonNegativeInteger
** nonPositiveInteger
** positiveInteger
* language
* long
** unsingedLong
* Name
* NCName
* NMTOKEN
* NMTOKENS
* NOTATION
* QName
* short
** unsignedShort
* string
** normalizedString
* time
* token

Other simple types are derived from built-in simple types.

Element =xsd:simpleType= with attribute =name=.

Element =xsd:restriction= with attribute =base= characterizing a built-in
simple type.

---++ Facets

Elements =xsd:minInclusive= and =xsd:maxInclusive= with attribute =value=.

@author Wouter Beek
@see http://www.w3.org/TR/2004/REC-xmlschema-0-20041028/
@tbd
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).

:- xml_register_namespace(w3c, 'http://www.w3.org/').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema').

init:-
  Graph = w3c,
  rdf_global_id(w3c:'TR/2004/REC-xmlschema-0-20041028/', This),
  rdfs_assert_individual(This, w3c:'Recommendation', Graph),
  rdf_assert_datatype(This, w3c:year, gYear, 2004, Graph),
  rdf_assert_literal(This, std:title, 'XML Schema Part 0: Primer Second Edition', Graph),
  rdf_assert_literal(This, w3c:author, 'David C. Fallside', Graph),
  rdf_assert_literal(This, w3c:author, 'Priscilla Walmsley', Graph),
  true.
:- init.

