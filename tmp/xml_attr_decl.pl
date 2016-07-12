:- module(
  xml_attr_decl,
  [
    'AttlistDecl'//2 % ?Version:oneof(['1.0','1.1'])
                     % ?AttributeDeclaration:compound
  ]
).

/** <module> XML: Attribute declaration

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2015/07, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_literal)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml10_code)).





%! 'AttDef'(?Version:oneof(['1.0','1.1']), ?AttributeDefinition:compound)// .
%
% ```bnf
% AttDef ::= S Name S AttType S DefaultDecl
% ```
%
% @compat XML 1.1.2 [53]

'AttDef'(Version, attr_def(AttributeName,Type,Default)) -->
  'S',
  'Name'(AttributeName),
  'S',
  'AttType'(Type),
  'S',
  'DefaultDecl'(Version, Default).



%! 'AttlistDecl'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?AttributeDeclaration:compound
%! )// .
%
% Defines the attributes of an element type.
%
% ```bnf
% AttlistDecl ::= '<!ATTLIST' S Name AttDef* S? '>'
% ```
%
% @compat XML 1.1.2 [52]

'AttlistDecl'(Version, attr_decl(ElementType,AttrDefs)) -->
  "<!ATTLIST",
  'S',
  'Name'(ElementType),
  '*'('AttDef'(Version), AttrDefs, []),
  ?('S'),
  ">".



%! 'AttType'(?Type)// .
%
% ```bnf
% AttType ::= StringType | TokenizedType | EnumeratedType
% ```
%
% @compat XML 1.1.2 [54]

'AttType'(Type) -->
  'StringType'(Type).
'AttType'(Type) -->
  'TokenizedType'(Type).
'AttType'(Type) -->
  'EnumeratedType'(Type).



%! 'DefaultDecl'(?Version:oneof(['1.0','1.1']), ?DefaultValue:compound)// .
%
% ```bnf
% DefaultDecl ::=   '#REQUIRED'
%                 | '#IMPLIED'
%                 | (('#FIXED' S)? AttValue)
% ```
%
% @compat XML 1.1.2 [60]
% @tbd [VC: Required Attribute]
% @tbd [VC: Attribute Default Value Syntactically Correct]
% @tbd [WFC: No < in Attribute Values]
% @tbd [VC: Fixed Attribute Default]
% @tbd [WFC: No External Entity References]

'DefaultDecl'(_, required) -->
  "#REQUIRED".
'DefaultDecl'(_, implied) -->
  "#IMPLIED".
'DefaultDecl'(Version, fixed(Value)) -->
  ("#FIXED", 'S' ; ""),
  'AttValue'(Version, Value).



%! 'EnumeratedType'(?Type:compound)// .
%
% ```bnf
% EnumeratedType ::= NotationType | Enumeration
% ```
%
% @compat XML 1.1.2 [57]

'EnumeratedType'(Type) --> 'NotationType'(Type).
'EnumeratedType'(Type) --> 'Enumeration'(Type).



%! 'Enumeration'(?Type:compound)// .
%
% ```bnf
% Enumeration ::= '(' S? Nmtoken (S? '|' S? Nmtoken)* S? ')'
% ```
%
% @compat XML 1.1.2 [59]
% @tbd [VC: Enumeration]
% @tbd [VC: No Duplicate Tokens]

'Enumeration'(oneof([Tokens])) -->
  "(",
  ?('S'),
  '+'('Nmtoken', Tokens, [separator(xml_pipe)]),
  ?('S'),
  ")".



%! 'NotationType'(?Names:list(atom))// .
%
% ```bnf
% NotationType ::= 'NOTATION' S '(' S? Name (S? '|' S? Name)* S? ')'
% ```
%
% @compat XML 1.1.2 [58]
% @tbd [VC: Notation Attributes]
% @tbd [VC: One Notation Per Element Type]
% @tbd [VC: No Notation on Empty Element]
% @tbd [VC: No Duplicate Tokens]

'NotationType'(Names) -->
  "NOTATION",
  'S',
  "(",
  ?('S'),
  '*'('Name', Names, [separator(xml_pipe)]),
  ?('S'),
  ")".



%! 'StringType'// .
%
% ```bnf
% StringType ::= 'CDATA'
% ```
%
% @compat XML 1.1.2 [55]

'StringType'(cdata) --> "CDATA".



%! 'TokenizedType'// .
%
% ```bnf
% TokenizedType ::=   'ID'
%                   | 'IDREF'
%                   | 'IDREFS'
%                   | 'ENTITY'
%                   | 'ENTITIES'
%                   | 'NMTOKEN'
%                   | 'NMTOKENS'
% ```
%
% @compat XML 1.1.2 [56]
% @tbd [VC: ID]
% @tbd [VC: One ID per Element Type]
% @tbd [VC: ID Attribute Default]
% @tbd [VC: IDREF] for 'IDREF' and 'IDREFS'
% @tbd [VC: Entity Name] for 'ENTITY' and 'ENTITIES'
% @tbd [VC: Name Token] for 'NMTOKEN' and 'NMTOKENS'

'TokenizedType'(id) --> "ID".
'TokenizedType'(idref) --> "IDREF".
'TokenizedType'(idrefs) --> "IDREFS".
'TokenizedType'(entity) --> "ENTITY".
'TokenizedType'(entities) --> "ENTITIES".
'TokenizedType'(nmtoken) --> "NMTOKEN".
'TokenizedType'(nmtokens) --> "NMTOKENS".
