:- module(
  xml_standalone,
  [
    'SDDecl'//1 % ?Standalone:boolean
  ]
).

/** <module> XML syntax: Standalone Document Declaration

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2015/07, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml_prolog)).
:- use_module(library(xml/xml10_code)).





%! 'SDDecl'(?Standalone:boolean)// .
% **Standalone Declaration**.
%
% ```bnf
% SDDecl ::= S 'standalone' Eq
%            (("'" ('yes' | 'no') "'") | ('"' ('yes' | 'no') '"'))
% ```
%
% @compat XML 1.0.5 [32].
% @compat XML 1.1.2 [32].
% @tbd [VC: Standalone Document Declaration]

'SDDecl'(Standalone) -->
  'S',
  "standalone",
  'Eq',
  quoted('y/n'(Standalone)).

'y/n'(true) --> "yes".
'y/n'(false) --> "no".
