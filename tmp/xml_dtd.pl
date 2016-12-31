:- module(
  xml_dtd,
  [
    markupdecl//2 % ?Version:oneof(['1.0','1.1'])
                  % ?Declaration:compound
  ]
).

/** <module> XML: DTD

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library/dcg_ext)).
:- use_module(library(xml/xml_attr_decl)).
:- use_module(library(xml/xml_comment)).
:- use_module(library(xml/xml_element_type_decl)).
:- use_module(library(xml/xml_entity_decl)).
:- use_module(library(xml/xml_entity_ref)).
:- use_module(library(xml/xml_notation_decl)).
:- use_module(library(xml/xml_pi)).
:- use_module(library(xml/xml_name_token)).
:- use_module(library(xml/xml10_code)).





%! markupdecl(?Version:oneof(['1.0','1.1']), ?Declaration:compound)// .
%
% ```bnf
% markupdecl ::=   elementdecl
%                | AttlistDecl
%                | EntityDecl
%                | NotationDecl
%                | PI
%                | Comment
% ```
%
% @compat XML 1.0.5 [29].
% @compat XML 1.1.2 [29].
% @tbd [WFC: PEs in Internal Subset]
% @tbd [VC: Proper Declaration/PE Nesting]

markupdecl(_, ElementDeclaration) -->
  elementdecl(ElementDeclaration).
markupdecl(Version, AttributeDeclaration) -->
  'AttlistDecl'(Version, AttributeDeclaration).
markupdecl(Version, EntityDeclaration) -->
  'EntityDecl'(Version, EntityDeclaration).
markupdecl(_, NotationDeclaration) -->
  'NotationDecl'(NotationDeclaration).
markupdecl(Version, Pi) -->
  'PI'(Version, Pi).
markupdecl(Version, _) -->
  'Comment'(Version).
