:- module(
  xml_notation_decl,
  [
    'NotationDecl'//1 % ?NotationDeclaration:compound
  ]
).

/** <module> XML: Notation declaration

**Notations** identify by name:
  - the format of unparsed entities,
  - the format of elements which bear a notation attribute, or
  - the application to which a processing instruction is addressed.

**Notation declarations** provide a name for the notation, for use in
 entity and attribute-list declarations and in attribute specifications,
 and an external identifier for the notation which may allow an XML
 processor or its client application to locate a helper application
 capable of processing data in the given notation.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_entity_decl)).
:- use_module(library(xml/xml_literal)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml10_code)).





%! 'NotationDecl'(?NotationDeclaration:compound)// .
% ```bnf
% NotationDecl ::= '<!NOTATION' S Name S (ExternalID | PublicID) S? '>'
% ```
%
% @compat XML 1.1.2 [82]
% @tbd [VC: Unique Notation Name]

'NotationDecl'(notation_decl(Name,Literal)) -->
  "<!NOTATION",
  'S',
  'Name'(Name),
  'S',
  ('ExternalID'(Literal) ; 'PublicID'(Literal)),
  ?('S'),
  ">".



%! 'PublicID'(?Literal:atom)// .
% ```bnf
% PublicID ::= 'PUBLIC' S PubidLiteral
% ```
%
% @compat XML 1.1.2 [83]

'PublicID'(Literal) -->
  "PUBLIC",
  'S',
  'PubidLiteral'(Literal).
