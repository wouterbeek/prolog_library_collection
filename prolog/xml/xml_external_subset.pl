:- module(
  xml_external_subset,
  [
    extSubsetDecl//2, % ?Version:oneof(['1.0','1.1'])
                      % ?Declarations:list(compound)
    'TextDecl'//3 % ?Version:oneof(['1.0','1.1'])
                  % ?XmlVersion:compound
                  % ?Encoding:atom
  ]
).

/** <module> XML: External subset

External parsed entities SHOULD each begin with a text declaration.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_conditional)).
:- use_module(library(xml/xml_dtd)).
:- use_module(library(xml/xml_prolog)).
:- use_module(library(xml/xml10_code)).





%! extSubset(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?XmlVersion:compound,
%!   ?Encoding:atom,
%!   ?Declarations:list(compound)
%! )// .
% ```bnf
% extSubset ::= TextDecl? extSubsetDecl
% ```
%
% @compat XML 1.0.5 [30].
% @compat XML 1.1.2 [30].

extSubset(Version, XmlVersion, Encoding, Declarations) -->
  '?'('TextDecl'(Version), XmlVersion, Encoding, []),
  extSubsetDecl(Version, Declarations).



%! extSubsetDecl(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?Declarations:list(compound)
%! )// .
% ```bnf
% extSubsetDecl ::= ( markupdecl | conditionalSect | DeclSep)*
% ```
%
% @compat XML 1.0.5 [31].
% @compat XML 1.1.2 [31].

extSubsetDecl(Version, [H|T]) -->
  markupdecl(Version, H),
  extSubsetDecl(Version, T).
extSubsetDecl(Version, [H|T]) -->
  conditionalSect(Version, H),
  extSubsetDecl(Version, T).
extSubsetDecl(Version, [H|T]) -->
  'DeclSep'(H),
  extSubsetDecl(Version, T).



%! 'TextDecl'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?XmlVersion:compound,
%!   ?Encoding:atom
%! )// .
% ```bnf
% TextDecl   ::= '<?xml' VersionInfo? EncodingDecl S? '?>'
% ```
%
% @compat XML 1.1.2 [77]

'TextDecl'(Version, XmlVersion, Encoding) -->
  "<?xml",
  ?('VersionInfo'(Version, XmlVersion)),
  'EncodingDecl'(Encoding),
  ?('S'),
  "?>".
