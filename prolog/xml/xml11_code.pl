:- module(
  xml11_code,
  [
    'Char'//1, % ?Code:code
    'RestrictedChar11'//1 % ?Code:code
  ]
).
:- reexport(library(xml/xml11_code), [
     'S'//0
   ]).

/** <module> XML 1.1: Codes

DCGs for character definitions in XML recommendations.

@author Wouter Beek
@compat XML 1.1.2
@see http://www.w3.org/TR/2006/REC-xml11-20060816/
@version 2015/07, 2015/11
*/





%! 'Char'(?Code:code)// .
% ```ebnf
% Char ::= [#x1-#xD7FF]
%        | [#xE000-#xFFFD]
%        | [#x10000-#x10FFFF]
%          /* any Unicode character, excluding the surrogate blocks,
%             FFFE, and FFFF. */
% ```

'Char'(C) -->
  [C],
  {(  between(0x1, 0xD7FF, C)
  ;   between(0xE000, 0xFFFD, C)
  ;   between(0x10000, 0x10FFFF, C)
  )}.



%! 'RestrictedChar'(?Code:code)// .
% ```ebnf
% RestrictedChar11 ::=
%     \\ Start of heading, start of text, end of text, end of transmission,
%     \\ enquiry, positive acknowledgement, bell, backspace.
%       [#x1-#x8]
%
%     \\ Vertical tab, form feed.
%     | [#xB-#xC]
%
%     \\ Shift out, shift in, data link escape, device control (1, 2, 3, 4),
%     \\ negative acknowledgement, synchronous idle,
%     \\ end of transmission block, cancel, end of medium, substitute,
%     \\ escape, file separator, group separator, record separator,
%     \\ unit separator.
%     | [#xE-#x1F]
%
%     | [#x7F-#x84]
%     | [#x86-#x9F]
% ```

'RestrictedChar11'(C) -->
  [C],
  {(  between(0x1,  0x8,  C)
  ;   between(0xB,  0xC,  C)
  ;   between(0xE,  0x1F, C)
  ;   between(0x7F, 0x84, C)
  ;   between(0x86, 0x9F, C)
  )}.
