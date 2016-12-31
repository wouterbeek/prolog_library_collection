:- module(
  xml_entity_ref,
  [
    'Reference'//2 % ?Version:oneof(['1.0','1.1'])
                   % ?Reference:atom
  ]
).

/** <module> XML: Entity reference

An **entity reference** refers to the content of a named entity.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11
*/

:- use_module(library(xml/xml_char_ref)).
:- use_module(library(xml/xml_name_token)).





%! 'EntityRef'(?EntityReference:atom)// .
%
% **Entity Reference**.
%
% ```bnf
% EntityRef ::= '&' Name ';'
% ```
%
% @compat XML 1.0.5 [68].
% @compat XML 1.1.2 [68].
% @tbd [WFC: Entity Declared]
% @tbd [VC: Entity Declared]
% @tbd [WFC: Parsed Entity]
% @tbd [WFC: No Recursion]

'EntityRef'(Name) -->
  "&",
  'Name'(Name),
  ";".



%! 'Reference'(?Version:oneof(['1.0','1.1']), ?Reference:atom)// .
% ```bnf
% Reference ::= EntityRef | CharRef
% ```
%
% @compat XML 1.0.5 [67].
% @compat XML 1.1.2 [67].

'Reference'(_, Reference) -->
  'EntityRef'(Reference).
'Reference'(Version, Reference) -->
  'CharRef'(Version, Reference).
