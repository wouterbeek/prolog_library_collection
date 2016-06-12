:- module(
  xml_prolog,
  [
    'EncodingDecl'//1, % ?Enc:atom
    'Eq'//0,
    'Misc'//2, % ?Version:oneof(['1.0','1.1'])
               % -ProcessingInstructions:list(compound)
    prolog//4, % ?Version:oneof(['1.0','1.1'])
               % ?XmlDecl:compound
               % ?DoctypeDecl:compound
               % -ProcessingInstructions:list(compound)
    'VersionInfo'//2, % ?Version:oneof(['1.0','1.1'])
                      % ?XmlVersion:compound
    'XMLDecl'//2 % ?Version:oneof(['1.0','1.1'])
                 % ?XmlDecl:compound
  ]
).

/** <module> XML syntax: Prolog

Grammar for the XML prolog.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2015/06, 2016/06
*/

:- use_module(library(apply)).
:- use_module(library(dcg/dcg_ext)).
:- use_module(library(lists)).
:- use_module(library(xml/xml_comment)).
:- use_module(library(xml/xml_dtd)).
:- use_module(library(xml/xml_pi)).
:- use_module(library(xml/xml_standalone)).
:- use_module(library(xml/xml10_code)).





%! 'EncodingDecl'(?Enc:atom)// .
% ```bnf
%    EncodingDecl ::= S 'encoding' Eq ('"' EncName '"' | "'" EncName "'" )
% ```
%
% @compat XML 1.1.2 [80]

'EncodingDecl'(Name) -->
  'S',
  "Encoding",
  'Eq',
  quoted('EncName'(Name)).



%! 'EncName'(?Enc:atom)// .
% Enc name contains only Latin characters.
%
% ```bnf
% EncName ::= [A-Za-z] ([A-Za-z0-9._] | '-')*
% ```
%
% compat XML 1.1.2 [81]

'EncName'(Name) -->
  {var(Name)}, !,
  ascii_letter(H),
  '*'('EncName_char', T),
  {atom_codes(Name, [H|T])}.
'EncName'(Name) -->
  {atom_codes(Name, [H|T])},
  ascii_letter(H),
  '*'('EncName_char', T).

'EncName_char'(Code) --> ascii_alpha_numeric(Code).
'EncName_char'(Code) --> dot(Code).
'EncName_char'(Code) --> underscore(Code).
'EncName_char'(Code) --> hyphen(Code).



%! 'Eq'// .
% ```bnf
% Eq ::= S? '=' S?
% ```
%
% @compat XML 1.0.5 [25].
% @compat XML 1.1.2 [25].

'Eq' -->
  ?('S'),
  "=",
  ?('S').



%! prolog(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?XmlDecl:compound,
%!   ?DoctypeDecl:compound,
%!   -ProcessingInstructions:list(compound)
%! )// .
% ```bnf
% prolog ::= XMLDecl? Misc* (doctypedecl Misc*)?
% ```
%
% @compat XML 1.0.5 [22].
% @compat XML 1.1.2 [22].

prolog(Version, XmlDecl, DoctypeDecl, Pis) -->
  ?('XMLDecl'(Version, XmlDecl)),
  '*'('Misc'(Version), Pis1),
  (   doctypedecl(Version, DoctypeDecl),
      '*'('Misc'(Version), Pis2)
  ;   ""
  ),
  {
    append(Pis1, Pis2, Pis3),
    exclude(var, Pis3, Pis)
  }.



%! 'Misc'(?Version:oneof(['1.0','1.1']), ?ProcessingInstructions:compound)// .
% ```bnf
% Misc ::= Comment | PI | S
% ```
%
% @compat XML 1.0.5 [27].
% @compat XML 1.1.2 [27].

'Misc'(Version, _) --> 'Comment'(Version).
'Misc'(Version, Pi) --> 'PI'(Version, Pi).
'Misc'(_, _) --> 'S'.



%! 'VersionInfo'(?Version:oneof(['1.0','1.1']), ?XmlVersion:compound)// .
% ```bnf
% VersionInfo ::= S 'version' Eq ("'" VersionNum "'" | '"' VersionNum '"')
% ```
%
% @compat XML 1.0.5 [24].
% @compat XML 1.1.2 [24].

'VersionInfo'(Version, XmlVersion) -->
  'S',
  "version",
  'Eq',
  quoted('VersionNum'(Version, XmlVersion)).



%! 'VersionNum'(
%!   ?Version:oneof(['1.0','1.1']),
%!   ?XmlVersion:compound
%! )// .
%
% ## XML 1.0.5
%
% ```bnf
% VersionNum ::= '1.' [0-9]+
% ```
%
% ## XML 1.1.2
%
% ```bnf
% VersionNum ::= '1.1'
% ```
%
% @compat XML 1.0.5 [26].
% @compat XML 1.1.2 [26].

'VersionNum'('1.0', version(1,Minor)) -->
  "1.",
  clpfd_positional(Minor, Ds),
  +(decimal_digit, Ds).
'VersionNum'('1.1', version(1,[1])) -->
  "1.1".



%! 'XMLDecl'(?Version:oneof(['1.0','1.1']), ?XmlDecl:compound)// .
% ```bnf
% XMLDecl ::= '<?xml' VersionInfo EncodingDecl? SDDecl? S? '?>'
% ```
%
% @compat XML 1.0.5 [23].
% @compat XML 1.1.2 [23].

'XMLDecl'(Version, xml_decl(XmlVersion,Enc,Standalone)) -->
  "<?xml",
  'VersionInfo'(Version, XmlVersion),
  ?('EncodingDecl', Enc),
  ?('SDDecl'(Standalone)),
  ?('S'),
  "?>".
