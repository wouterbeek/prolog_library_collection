:- module(
  xml10_code,
  [
    'Char'//1, % ?Code:code
    'S'//0
  ]
).

/** <module> XML 1.0: Codes

@author Wouter Beek
@compat XML 1.0.5
@see http://www.w3.org/TR/2008/REC-xml-20081126/
@version 2015/11
*/

:- use_module(library(dcg/dcg_ext)).





%! 'Char'(?Code:code)// .
% An **XML Character** is an atomic unit of text specified by ISO/IEC 10646.
%
% ```ebnf
% Char ::=   #x9              // Horizontal tab
%          | #xA              // Line feed
%          | #xD              // Carriage return
%          | [#x20-#xD7FF]    // Space, punctuation, numbers, letters
%          | [#xE000-#xFFFD]
%          | [#x10000-#x10FFFF]
% ```
%
% Avoid comapatibility characters [Unicode, section 2.3].
% Avoid the following characters (control characters,
% permanently undefined Unicode characters):
%
% ```
% [#x7F-#x84] // Delete, ...
% [#x86-#x9F]
% [#xFDD0-#xFDEF],
% [#x1FFFE-#x1FFFF]
% [#x2FFFE-#x2FFFF]
% [#x3FFFE-#x3FFFF]
% [#x4FFFE-#x4FFFF]
% [#x5FFFE-#x5FFFF]
% [#x6FFFE-#x6FFFF]
% [#x7FFFE-#x7FFFF]
% [#x8FFFE-#x8FFFF]
% [#x9FFFE-#x9FFFF]
% [#xAFFFE-#xAFFFF]
% [#xBFFFE-#xBFFFF]
% [#xCFFFE-#xCFFFF]
% [#xDFFFE-#xDFFFF]
% [#xEFFFE-#xEFFFF]
% [#xFFFFE-#xFFFFF]
% [#x10FFFE-#x10FFFF]
% ```

'Char'(0x9) --> [0x9].
'Char'(0xA) --> [0xA].
'Char'(0xD) --> [0xD].
'Char'(C) -->
  [C],
  {(  between(0x20, 0xD7FF, C)
  ;   between(0xE000, 0xFFFD, C)
  ;   between(0x10000, 0x10FFFF, C)
  )}.



%! 'S'// .
% White space.
%
% ```ebnf
% S ::= ( #x20 | #x9 | #xD | #xA )+   // Any consecutive number of spaces,
%                                     // carriage returns, line feeds, and
%                                     // horizontal tabs.
% ```
%
% The presence of carriage_return// in the above production is maintained
% purely for backward compatibility with the First Edition.
% All `#xD` characters literally present in an XML document are either removed
% or replaced by line_feed// (i.e., `#xA`) characters before any other
% processing is done.

'S' --> +(s).
s --> [0x20].
s --> [0x9].
s --> [0xD].
s --> [0xA].
