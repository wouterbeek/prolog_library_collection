:- module(
  xml_name_token,
  [
    'Name'//1, % ?Name:atom
    'Names'//1, % ?Names:list(atom)
    'Nmtoken'//1, % ?Token:atom
    'Nmtokens'//1 % ?Tokens:list(atom)
  ]
).

/** <module> XML name token

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/03-2014/05, 2014/10-2014/12, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(dcg/dcg_unicode)).





%! 'Name'(?Name:atom)//
% An **XML Name** is an *Nmtoken*
%  with a restricted set of initial characters.
%
% Disallowed initial characters for names include digits, diacritics,
% the full stop and the hyphen.
%
% ```bnf
% Name ::= NameStartChar (NameChar)*
% ```
%
% ## Reserved names
%
% Names beginning with `(x,m,l)` are reserved for standardization in this
% or future versions of this specification.
%
% ## XML Namespaces
%
% The Namespaces in XML Recommendation assigns a meaning to names containing
% colon characters. Therefore, authors should not use the colon in XML names
% except for namespace purposes, but XML processors must accept the colon as
% a name character.
%
% @compat XML 1.0.5 [5].
% @compat XML 1.1.2 [5].

'Name'(Name) -->
  dcg_atom('Name_codes', Name).

'Name_codes'([H|T]) -->
  'NameStartChar'(H),
  '*'('NameChar', T, []).



%! 'NameChar'(?C)// .
% ```ebnf
% NameChar ::=   NameStartChar
%              | "-"
%              | "."
%              | [0-9]
%              | #xB7
%              | [#x0300-#x036F]
%              | [#x203F-#x2040]
% ```
%
% @compat XML 1.0.5 [4a].
% @compat XML 1.1.2 [4a].

'NameChar'(C) -->
  'NameStartChar'(C).
'NameChar'(C) -->
  hyphen_minus(C).
'NameChar'(C) -->
  dot(C).
'NameChar'(C) -->
  decimal_digit(_, C).
% #x00B7
'NameChar'(C) -->
  middle_dot(C).
% #x0300-#x036F
'NameChar'(C) -->
  between_code_rad(hex('0300'), hex('036F'), dec(C)).
% #x203F
'NameChar'(C) -->
  undertie(C).
% #x2040
'NameChar'(C) -->
  character_tie(C).



%! 'NameStartChar'(?C)// .
% ```ebnf
% NameStartChar ::=   ":"
%                   | [A-Z]
%                   | "_"
%                   | [a-z]
%                   | [#xC0-#xD6]
%                   | [#xD8-#xF6]
%                   | [#xF8-#x2FF]
%                   | [#x370-#x37D]
%                   | [#x37F-#x1FFF]
%                   | [#x200C-#x200D]
%                   | [#x2070-#x218F]
%                   | [#x2C00-#x2FEF]
%                   | [#x3001-#xD7FF]
%                   | [#xF900-#xFDCF]
%                   | [#xFDF0-#xFFFD]
%                   | [#x10000-#xEFFFF]
% ```
%
% @compat XML 1.0.5 [4].
% @compat XML 1.1.2 [4].

% [A-Z] and [a-z]
'NameStartChar'(C) -->
  ascii_letter(C).
% ":"
'NameStartChar'(C) -->
  colon(C).
% "_"
'NameStartChar'(C) -->
  underscore(C).
% #xC0-#xD6
'NameStartChar'(C) -->
  between_code_rad(hex('C0'), hex('D6'), dec(C)).
% #xD8-#xF6
'NameStartChar'(C) -->
  between_code_rad(hex('D8'), hex('F6'), dec(C)).
% #xF8-#x2FF
'NameStartChar'(C) -->
  between_code_rad(hex('F8'), hex('2FF'), dec(C)).
% #x370-#x37D
'NameStartChar'(C) -->
  between_code_rad(hex('370'), hex('37D'), dec(C)).
% #x37F-#x1FFF
'NameStartChar'(C) -->
  between_code_rad(hex('37F'), hex('1FFF'), dec(C)).
% #x200C-#x200D
'NameStartChar'(C) -->
  zero_width_non_joiner(C).
'NameStartChar'(C) -->
  zero_width_joiner(C).
% #x2070-#x218F
'NameStartChar'(C) -->
  between_code_rad(hex('2070'), hex('218F'), dec(C)).
% #x2C00-#x2FEF
'NameStartChar'(C) -->
  between_code_rad(hex('2C00'), hex('2FEF'), dec(C)).
% #x3001-#xD7FF
'NameStartChar'(C) -->
  between_code_rad(hex('3001'), hex('D7FF'), dec(C)).
% #xF900-#xFDCF
'NameStartChar'(C) -->
  between_code_rad(hex('F900'), hex('FDCF'), dec(C)).
% #xFDF0-#xFFFD
'NameStartChar'(C) -->
  between_code_rad(hex('FDF0'), hex('FFFD'), dec(C)).
% #x10000-#xEFFFF
'NameStartChar'(C) -->
  between_code_rad(hex('10000'), hex('EFFFF'), dec(C)).



%! 'Names'(?Names:list(atom))// .
% ```ebnf
% Names ::= Name (#x20 Name)*
% ```
%
% @compat XML 1.0.5 [6].
% @compat XML 1.1.2 [6].

'Names'(Names) -->
  seplist('Name', " ", Names).



%! 'Nmtoken'(?Token:atom)// .
% ```ebnf
% Nmtoken ::= (NameChar)+
% ```
%
% @compat XML 1.0.5 [7].
% @compat XML 1.1.2 [7].

'Nmtoken'(Token) -->
  dcg_atom('Nmtoken_codes', Token).

'Nmtoken_codes'(Cs) -->
  +('NameChar', Cs).



%! 'Nmtokens'(?Tokens:list(atom))// .
% ```ebnf
% Nmtokens ::= Nmtoken (#x20 Nmtoken)*
% ```
%
% @compat XML 1.0.5 [8].
% @compat XML 1.1.2 [8].

'Nmtokens'(Tokens) -->
  seplist('Nmtoken', " ", Tokens).
