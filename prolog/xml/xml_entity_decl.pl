:- module(
  xml_entity_decl,
  [
    'EntityDecl'//2, % ?Version:oneof(['1.0','1.1'])
                     % ?EntityDeclaration:compound
    'EntityDef'//2, % ?Version:oneof(['1.0','1.1'])
                    % ?EntityDefinition:compound
    'ExternalID'//1, % ?ExternalId:compound
    'PEDef'//2 % ?Version:oneof(['1.0','1.1'])
               % ?ParameterEntityDefinition:compound
  ]
).

/** <module> XML: Entity Declaration

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_literal)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml10_code)).





%! 'EntityDecl'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?EntityDeclaration:compound
%! )// .
% ```bnf
%    EntityDecl ::= GEDecl | PEDecl
% ```
%
% @compat XML 1.0.5 [70]
% @compat XML 1.1.2 [70]

'EntityDecl'(Version, GeneralEntityDeclaration) -->
  'GEDecl'(Version, GeneralEntityDeclaration).
'EntityDecl'(Version, ParameterEntityDeclaration) -->
  'PEDecl'(Version, ParameterEntityDeclaration).



%! 'EntityDef'(?Version:oneof(['1.0','1.1']), ?EntityDefinition:compound)// .
% ```bnf
% EntityDef ::= EntityValue | (ExternalID NDataDecl?)
% ```
%
% @compat XML 1.0.5 [73]

'EntityDef'(Version, entity_def(Literal)) -->
  'EntityValue'(Version, Literal).
'EntityDef'(_, entity_def(ExternalId,NotationName)) -->
  'ExternalID'(ExternalId),
  '?'('NDataDecl', NotationName, []).



%! 'ExternalID'(?ExternalId:compound)// .
% ```bnf
% ExternalID ::=   'SYSTEM' S SystemLiteral
%                | 'PUBLIC' S PubidLiteral S SystemLiteral
% ```
%
% @compat XML 1.0.5 [75]
% @compat XML 1.1.2 [75]

'ExternalID'(external_id(system,Literal)) -->
  "SYSTEM",
  'S',
  'SystemLiteral'(Literal).
'ExternalID'(external_id(public,Literal1,Literal2)) -->
  "PUBLIC",
  'S',
  'PubidLiteral'(Literal1),
  'S',
  'SystemLiteral'(Literal2).



%! 'GEDecl'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?GeneralEntityDeclaration:compound
%! )// .
% **General Entity Declaration**.
%
% ```bnf
% GEDecl ::= '<!ENTITY' S Name S EntityDef S? '>'
% ```
%
% @compat XML 1.0.5 [71]

'GEDecl'(Version, general_entity_decl(EntityName,EntityDefinition)) -->
  "<!ENTITY",
  'S',
  'Name'(EntityName),
  'S',
  'EntityDef'(Version, EntityDefinition),
  ?('S'),
  ">".



%! 'NDataDecl'(?NotationName:atom)// .
% **Notation Data Declaration**.
%
% ```bnf
%    NDataDecl ::= S 'NDATA' S Name
% ```
%
% @compat XML 1.0.5 [76]
% @compat XML 1.1.2 [76]
% @tbd [VC: Notation Declared]

'NDataDecl'(NotationName) -->
  'S',
  "NDATA",
  'S',
  'Name'(NotationName).



%! 'PEDecl'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?ParameterEntityDeclaration:compound
%! )//
%  . Parameter Entity declaration.
%
% ```bnf
% PEDecl ::= '<!ENTITY' S '%' S Name S PEDef S? '>'
% ```
%
% @compat XML 1.0.5 [72]

'PEDecl'(Version, pe_decl(Name,ParameterEntityDefinition)) -->
  "<!ENTITY",
  'S',
  "%",
  'S',
  'Name'(Name),
  'S',
  'PEDef'(Version, ParameterEntityDefinition),
  ?('S'),
  ">".



%! 'PEDef'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?ParameterEntityDefinition:compound
%! )// .
% ```bnf
% PEDef ::=    EntityValue | ExternalID
% ```
%
% @compat XML 1.0.5 [74]

'PEDef'(Version, Names) --> 'EntityValue'(Version, Names).
'PEDef'(_, ExternalId) --> 'ExternalID'(ExternalId).
