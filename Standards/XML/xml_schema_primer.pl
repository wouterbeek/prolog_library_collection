:- module(xml_schema_primer, []).

/** <module> XML SCHEMA PRIMER



# Attribute

An attribute can occur at most once for an element.

Default values are given for missing attributes that have optional use
and a set default value.

*|Global attribute|*: an attribute whose declaration is a child of
=xsd:schema=.


## Attribute declaration

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



# Complex types

Complex types can contain element declarations, element references, and
attribute declarations.

Complex types are defined using the =xsd:complexType= element,
with attribute =name=.


# Complex types with simple content

A type that allows an element to have attributes and contain a simple value.

A =xsd:complexType= with =xsd:simpleContent= (only character data; no
subelements), with =xsd:extension= with attribute =base=.

Example:
~~~{.xml}
<xsd:complexType name="SOME-COMPLEX-TYPE">
  <xsd:simpleContent>
    <xsd:extension base="xsd:decimal">
      <xsd:attribute name="SOME-ATTRIBUTE" type="SOME-SIMPLE-TYPE">
    </xsd:extension>
  </xsd:simpleContent>
</xsd:complexType>
~~~


## Complex types with mixed content

Content that consits of both subelements and character data.

Use attribute =mixed= with value =true= on element =xsd:complexType=.

Example:
~~~{.xml}
<xsd:complexType mixed="true" name="SOME-COMPLEX-TYPE">
  <xsd:sequence>
    <xsd:element name="SOME-ELEMENT">
  </xsd:sequence>
</xsd:complexType>
~~~

## Complex types with empty content

Element =xsd:complexContent= can only containt elements, but we define no
elements in it. We can add attributes by restricting an existing datatype.

The following pattern:
~~~{.xml}
<xsd:complexType name="SOME-COMPLEX-TYPE">
  <xsd:complexContent>
    <xsd:restriction base="xsd:anyType">
      <attribute name="SOME-ATTRIBUTE" type="SOME-SIMPLE-DATATYPE">
    <xsd:restriction>
  </xsd:complexContent>
</xsd:complexType>
~~~
can be abbreviated as follows:
~~~{.xml}
<xsd:complexType name="SOME-COMPLEX-TYPE">
  <xsd:attribute name="SOME-ATTRIBUTE" type="SOME-SIMPLE-DATATYPE">
</xsd:complexType>
~~~




# Element

Default values are set for missing elements that have optional use
and a set default value and that do occur as empty elements.
(Missing elements do not receive their default value.)

*|Global element|*: an element whose declaration is a child of =xsd:schema=.
Global elements can appear at the top-level of an instance document.


## Element declaration

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



# Simple types

We distinguish between *|build-in types|* and *|defined types|*.


## Atomic types

The atomic types are all built-in simple types:
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
* float
* gDay
* gMonth
* gMonthDay
* gYear
* gYearMonth
* hexBinary
* ID
* IDREF
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
* NOTATION
* QName
* short
** unsignedShort
* string
** normalizedString
* time
* token

New atomic types can be defined:
  * Element =xsd:simpleType= with attribute =name=.
  * Element =xsd:restriction= with attribute =base= characterizing a built-in
    simple type. Subelements are facets.

Applicable facets:
  * =[min|max][Exclusive|Inclusive]=
  * =pattern=

Multiple occurrences of element =xsd:enumeration= with attribute =value=.

Booleans cannot be faceted by enumeration.

Examples of defined atomic types:
~~~{.xml}
<xsd:simpleType name="myInteger">
  <xsd:restriction base="xsd:integer">
    <xsd:minInclusive value="10000"/>
    <xsd:maxInclusive value="99999"/>
  </xsd:restriction>
</xsd:simpleType>

<xsd:simpleType name="SKU">
  <xsd:restriction base="xsd:string">
    <xsd:pattern value="\d{3}-[A-Z]{2}"/>
  </xsd:restriction>
</xsd:simpleType>

<xsd:simpleType name="USState">
  <xsd:restriction base="xsd:string">
    <xsd:enumeration value="AK"/>
    <xsd:enumeration value="AL"/>
    <xsd:enumeration value="AR"/>
    <!-- and so on ... -->
  </xsd:restriction>
</xsd:simpleType>
~~~


## List types

There are 3 buil-in list types:
* =ENTITIES=
* =IDREFS=
* =NMTOKENS=

New list types can be defined:
  * Element =xsd:list= with attribute =itemType= with a simple type as value.

Applicable facets:
  * =enumeration=
  * =length=
  * =maxLength=
  * =minLength=
  * =patteren=

If strings contains whitespace, then the number of items in a list of
strings may exceed the number of strings that were put in that list.

Examples of defined list types:
~~~{.xml}
<xsd:simpleType name="USStateList">
  <xsd:list itemType="USState"/>
</xsd:simpleType>

<xsd:simpleType name="SixUSStates">
  <xsd:restriction base="USStateList">
    <xsd:length value="6"/>
  </xsd:restriction>
</xsd:simpleType>
~~~


## Union types

Element =xsd:union= with value =memberTypes= with a list of simple type names
as value.

Applicable facets:
  * =enumeration=
  * =pattern=

Example of a defined union type:
~~~{.xml}
<xsd:simpleType name="zipUnion">
  <xsd:union memberTypes="USState listOfMyIntType"/>
</xsd:simpleType>
~~~

# Anonymous types

A defined type that is references once can be defined anonymously,
i.e. as a sirect subelement of an element.

Expample of 2 anonymou types:
~~~{.xml}
<xsd:complexType name="Items">
  <xsd:sequence>
    <xsd:element name="item" minOccurs="0" maxOccurs="unbounded">
      <xsd:complexType>
        <xsd:sequence>
          <xsd:element name="productName" type="xsd:string"/>
          <xsd:element name="quantity">
            <xsd:simpleType>
              <xsd:restriction base="xsd:positiveInteger">
                <xsd:maxExclusive value="100"/>
              </xsd:restriction>
            </xsd:simpleType>
          </xsd:element>
          <xsd:element name="USPrice"  type="xsd:decimal"/>
          <xsd:element ref="comment"   minOccurs="0"/>
          <xsd:element name="shipDate" type="xsd:date" minOccurs="0"/>
        </xsd:sequence>
        <xsd:attribute name="partNum" type="SKU" use="required"/>
      </xsd:complexType>
    </xsd:element>
  </xsd:sequence>
</xsd:complexType>
~~~

# ur-type

The base type from which all simple and complex types are derived, called
=xsd:anyType=.

The default value type for elements.

Does not constrain the content in any way (i.e. allowing arbitrary character
and element content).



# Annotations

=xsd:annotation= elements may appear at the beginning of most schema
constructs. They may contain subelements =xsd:documentation= (for human
consumption) and =xsd:appinfo= (for machine consumption).



# Groups

Disjunctive specification of subelements: element =xsd:choice=.

Conjunctive specification of subelements: element =xsd:group=. This allows
disjunctions of conjunctions of length >1.

A named group is defined by an element =xsd:group= with attribute =id=.
The value of =id= can be refered to by attribute =ref= of element
=xsd:group= inside a complex type specification.

Examples:
~~~{.xml}
<xsd:complexType name="PurchaseOrderType">
  <xsd:sequence>
    <xsd:choice>
      <xsd:group   ref="shipAndBill"/>
      <xsd:element name="singleUSAddress" type="USAddress"/>
    </xsd:choice>
    <xsd:element ref="comment" minOccurs="0"/>
    <xsd:element name="items"  type="Items"/>
  </xsd:sequence>
  <xsd:attribute name="orderDate" type="xsd:date"/>
</xsd:complexType>

<xsd:group id="shipAndBill">
  <xsd:sequence>
    <xsd:element name="shipTo" type="USAddress"/>
    <xsd:element name="billTo" type="USAddress"/>
  </xsd:sequence>
</xsd:group>
~~~

## 'All' group

The =xsd:all= element specifies a group every subelement of which must occur
at most once.
This can only be used at the top-level of any content model. [???]
An =all= group must be the sole child of a content model.
The subelements must be individual elements / cannot themselves be groups.
Subelements cannot occur more than once (restrictions on the legal values of
=[max|min]Occurs=).

~~~{.xml}
<xsd:complexType name="PurchaseOrderType">
  <xsd:all>
    <xsd:element name="shipTo" type="USAddress"/>
    <xsd:element name="billTo" type="USAddress"/>
    <xsd:element ref="comment" minOccurs="0"/>
    <xsd:element name="items"  type="Items"/>
  </xsd:all>
  <xsd:attribute name="orderDate" type="xsd:date"/>
</xsd:complexType>
~~~

## Attribute groups

Attributes can be grouped in an =xsd:attributeGroup= element.

Named attribute groups can be defined using the =id= attribute.
The =ref= attribute in =xsd:attributeGroup= elements can reference these.

Attribute groups must occur at the end of complex type definitions.

# Nil mechanism

To explicitly indicate that an element has no content, rather than leaving
the element out, the nil value can be used.

The use of nil values is declared by setting the =nillable= attribute of
element =xsd:element= to true.

Nil values are used by setting the =xsi:nil= attribute to true.

Example:
~~~{.xml}
<xsd:element name="SOME-ELEMENT" type="SOME-TYPE" nillable="true"/>

<SOME-ELEMENT xsi:nil="true"></SOME-ELEMENT>
~~~

Nil values can only be defined for element values (not attribute values).

@author Wouter Beek
@see http://www.w3.org/TR/2004/REC-xmlschema-0-20041028/
@tbd
@version 2013/05
*/

:- use_module(library(semweb/rdf_db)).
:- use_module(rdf(rdf_build)).
:- use_module(rdfs(rdfs_build)).
:- use_module(xml(xml_namespace)).

:- xml_register_namespace(w3c, 'http://www.w3.org/').
:- xml_register_namespace(xsd, 'http://www.w3.org/2001/XMLSchema#').
% XML Scheme namespace for instances.
:- xml_register_namespace(xsi, 'http://www.w3.org/2001/XMLSchema-instance#').

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

