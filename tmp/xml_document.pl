:- module(
  xml_document,
  [
    document//5, % ?Version:oneof(['1.0','1.1']),
                 % ?XmlDeclaration:compound
                 % ?DoctypeDeclaration:compound
                 % -ProcessingInstructions:list(compound)
                 % ?RootElement:compound
    extParsedEnt//4 % ?Version:oneof(['1.0','1.1'])
                    % ?XmlVersion:compound
                    % ?Encoding:atom
                    % ?Content
  ]
).

/** <module> XML: Document

The XML (Extensible Markup Language) is a subset of
SGML (Standard Generalized Markup Language).

The document entity is well-formed if it matches the production labeled
 document//1.

An external general parsed entity is well-formed if it matches the production
 labeled extParsedEnt//.

# Concepts

  * **Document element**
    The single element in an XML document that has no parent element.
  * **Logical structure**
    The declarations, elements, comments, characters references, and
    processing instructions of an XML document.
  * **Physical structure**
    The entities / units that compose and XML document.
  * *Root*
    Synonym of _|document element|_.
  * *Validity*
    The property that an XML document complies with the constraints
    expressed in the document type declaration it references.
  * *Well-formedness*
    The property that an XML document matches the productions in the XML
    specification, meets all the well-formedness constraints, and contains
    only parsed entities that are well-formed.
  * **XML document**
    Can be split up in logical and physical structure.
  * **XML processor**
    A software module that can access the content and structure of
    XML documents.

---

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/10-2014/11, 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(xml/xml_element)).
:- use_module(library(xml/xml_external_subset)).
:- use_module(library(xml/xml_prolog)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml10_code)).





%! document(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?XmlDeclaration:compound,
%!   ?DoctypeDeclaration:compound,
%!   -ProcessingInstructions:list(compound),
%!   ?RootElement:compound
%! )// .
% # Syntax
%
% XML documents SHOULD begin with an XML declaration which specifies
%  the version of XML being used.
%
% ## XML 1.0.5
%
% ```bnf
% document ::= prolog element Misc*
% ```
%
% ## XML 1.1.2
%
% ```bnf
% document ::= ( prolog element Misc* ) - ( Char* RestrictedChar Char* )
% ```
%
% ### Example
%
% ```xml
% <?xml version="1.0"?>
% <greeting>Hello, world!</greeting>
% ```
%
% @compat XML 1.0.5 [1].
% @compat XML 1.1.2 [1].

document(Version, XmlDecl, DoctypeDecl, Pis, RootElement) -->
  {Version = '1.0'},
  prolog(Version, XmlDecl, DoctypeDecl, Pis1),
  element(Version, RootElement),
  '*'('Misc'(Version), Pis2, []),
  {
    exclude(var, Pis2, Pis3),
    append(Pis1, Pis3, Pis)
  }.
document(Version, XmlDecl, DoctypeDecl, Pis, X, Y):-
  Version = '1.1',
  document('1.0', XmlDecl, DoctypeDecl, Pis, X, Y),
  append(Codes, Y, X),
  \+ ((
    member(Code, Codes),
    'RestrictedChar'(Version, Code, _, _)
  )).



%! extParsedEnt(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?XmlVersion:compound,
%!   ?Encoding:atom,
%!   ?Content
%! )// .
% ```bnf
% extParsedEnt ::= ( TextDecl? content ) - ( Char* RestrictedChar Char* ) 
% ```
%
% @compat XML 1.1.2 [78]

extParsedEnt(Version, XmlVersion, Encoding, Content) -->
  {Version = '1.0'},
  ?('TextDecl'(Version), XmlVersion, Encoding),
  content(Version, Content).
extParsedEnt(Version, XmlVersion, Encoding, Content, X, Y):-
  Version = '1.1',
  extParsedEnt('1.0', XmlVersion, Encoding, Content, X, Y),
  append(Codes, Y, X),
  \+ ((
    member(Code, Codes),
    'RestrictedChar'(Version, Code, _, _)
  )).
