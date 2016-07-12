:- module(
  xml_char_ref,
  [
    'CharRef'//2 % ?Version:oneof(['1.0','1.1'])
                 % ?Code:code
  ]
).

/** <module> XML character reference

A **character reference** refers to a specific character in the ISO/IEC 10646
 character set, for example one not directly accessible from available input
 devices.

@author Wouter Beek
@compat [XML 1.0.5](http://www.w3.org/TR/2008/REC-xml-20081126/)
@compat [XML 1.1.2](http://www.w3.org/TR/2006/REC-xml11-20060816/)
@version 2014/11, 2015/06, 2016/06
*/

:- use_module(library(dcg/dcg_ext)).
:- use_module(library(xml/xml10_code)).





%! 'CharRef'(?Version:oneof(['1.0','1.1']), ?Code:code)// .
% **Character Reference**.
%
% # Syntax
%
% ```abnf
%   CharRef ::=   '&#' [0-9]+ ';'
%             | '&#x' [0-9a-fA-F]+ ';'
% ```
%
% ## Well-formedness constraint: Legal Character
%
% Characters referred to using character references MUST match
%  the production for Char//2.
%
% If the character reference begins with `&#x`, the digits and letters
%  up to the terminating `;` provide a hexadecimal representation of
%  the character's code point in ISO/IEC 10646.
% If it begins just with `&#`, the digits up to the terminating `;`
%  provide a decimal representation of the character's code point.
%
% @compat XML 1.0.5 [66]
% @compat XML 1.1.2 [66]

'CharRef'(Version, Code) -->
  "&#",
  {clpfd_positional(Code, Decs)},
  '+'(decimal_digit, Decs, []),
  ";",
  {'Char'(Version, Code, _, _)}.
'CharRef'(Version, Code) -->
  "&#x",
  {clpfd_positional(Code, Hexs)},
  '+'(hexadecimal_digit, Hexs, []),
  ";",
  {'Char'(Version, Code, _, _)}.
