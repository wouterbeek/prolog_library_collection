:- module(
  rfc2234,
  [
    'ALPHA'//0,
    'ALPHA'//1, % ?Code:code
    'BIT'//0,
    'BIT'//1, % ?Weight:between(0,1)
    'BIT'//2, % ?Weight:between(0,1)
              % ?Code:code
    'CHAR'//0,
    'CHAR'//1, % ?Code:code
    'CR'//0,
    'CR'//1, % ?Code:code
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:code
    'DIGIT'//0,
    'DIGIT'//1, % ?Weight:between(0,9)
    'DIGIT'//2, % ?Weight:between(0,9)
                % ?Code:code
    'DQUOTE'//0,
    'DQUOTE'//1, % ?Code:code
    'HEXDIG'//0,
    'HEXDIG'//1, % ?Weight:between(0,15)
    'HEXDIG'//2, % ?Weight:between(0,15)
                % ?Code:code
    'HTAB'//0,
    'HTAB'//1, % ?Code:code
    'LF'//0,
    'LF'//1, % ?Code:code
    'LWSP'//0,
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1, % ?Code:code
    'VCHAR'//0,
    'VCHAR'//1, % ?Code:code
    'WSP'//0,
    'WSP'//1 % ?Code:code
  ]
).

/** <module> RFC 2234: Augmented BNF for Syntax Specifications: ABNF

@author Wouter Beek
@compat RFC 2234
@deprecated Use `library(dcg/rfcXXX)` instead.
@version 2015/11
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_code)).





%! 'ALPHA'// .
%! 'ALPHA'(?Code:code)// .
% Alphabetic character, i.e. belonging to
% the range of ASCII letters, `A-Z / a-z`.
%
% Hexadecimal character range: `%x41-5A` through `%x61-7A`.
%
% ```abnf
% ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
% ```

'ALPHA' --> 'ALPHA'(_).
'ALPHA'(C) --> ascii_letter(C).



%! 'BIT'// .
%! 'BIT'(?Weight:between(0,1))// .
%! 'BIT'(?Weight:between(0,1), ?Code:code)// .
% A binary digit, i.e. `0` or `1`.
%
% ```abnf
% BIT = "0" / "1"
% ```

'BIT' --> 'BIT'(_).
'BIT'(W) --> 'BIT'(W, _).
'BIT'(0, 0'0) --> "0".
'BIT'(1, 0'1) --> "1".



%! 'CHAR'// .
%! 'CHAR'(?Code:code)// .
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% # Syntax
%
% ```abnf
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ```

'CHAR' --> 'CHAR'(_).
'CHAR'(C) --> between_code_radix(hex('01'), hex('7F'), C).



%! 'CR'// .
%! 'CR'(?Code:code)// .
% The carriage return.
%
% ```abnf
% CR = %x0D   ; carriage return
% ```

'CR' --> 'CR'(_).
'CR'(C) --> code_radix(hex('0D'), C).



%! 'CRLF'// .
% Internet standard newline.
%
% ```abnf
% CRLF = CR LF   ; Internet standard newline
% ```

'CRLF' --> 'CR', 'LF'.



%! 'CTL'// .
%! 'CTL'(?Code:code)// .
% Control character.
%
% # Syntax
%
% ```abnf
% CTL = %x00-1F / %x7F   ; controls
% ```

'CTL' --> 'CTL'(_).
'CTL'(C) --> between_code_radix(hex('00'), hex('1F'), C).
'CTL'(C) --> code_radix(hex('7F'), C).



%! 'DIGIT'// .
%! 'DIGIT'(?Weight:between(0,9))// .
%! 'DIGIT'(?Weight:between(0,9), ?Code:code)// .
% Decimal digit.
%
% # Syntax
%
% ```abnf
% DIGIT = %x30-39   ; 0-9
% ```

'DIGIT' --> 'DIGIT'(_).
'DIGIT'(W) --> 'DIGIT'(W, _).
'DIGIT'(0, 0'0) --> "0".
'DIGIT'(1, 0'1) --> "1".
'DIGIT'(2, 0'2) --> "2".
'DIGIT'(3, 0'3) --> "3".
'DIGIT'(4, 0'4) --> "4".
'DIGIT'(5, 0'5) --> "5".
'DIGIT'(6, 0'6) --> "6".
'DIGIT'(7, 0'7) --> "7".
'DIGIT'(8, 0'8) --> "8".
'DIGIT'(9, 0'9) --> "9".



%! 'DQUOTE'// .
%! 'DQUOTE'(?Code:code)// .
% US-ASCII double-quote mark.
%
% ```abnf
% DQUOTE = %x22   ; " (Double Quote)
% ```

'DQUOTE' --> 'DQUOTE'(_).
'DQUOTE'(C) --> code_radix(hex('22'), C).



%! 'HEXDIG'// .
%! 'HEXDIG'(?Weight:between(0,15))// .
%! 'HEXDIG'(?Weight:between(0,15), ?Code:code)// .
% Uppercase-only notation for hexadecimal digits.
%
% ```abnf
% HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ```

'HEXDIG' --> 'HEXDIG'(_).
'HEXDIG'(W) --> 'HEXDIG'(W, _).
'HEXDIG'(W, C) --> 'DIGIT'(W, C).
'HEXDIG'(10, 0'A) --> "A".
'HEXDIG'(11, 0'B) --> "B".
'HEXDIG'(12, 0'C) --> "C".
'HEXDIG'(13, 0'D) --> "D".
'HEXDIG'(14, 0'E) --> "E".
'HEXDIG'(15, 0'F) --> "F".



%! 'HTAB'// .
%! 'HTAB'(?Code:code)// .
% The horizontal tab.
%
% ```abnf
% HTAB = %x09   ; horizontal tab
% ```

'HTAB' --> 'HTAB'(_).
'HTAB'(C) --> code_radix(hex('09'), C).



%! 'LF'// .
%! 'LF'(?Code:code)// .
% The linefeed.
%
% # Syntax
%
% ```abnf
% LF = %x0A   ; linefeed
% ```

'LF' --> 'LF'(_).
'LF'(C) --> code_radix(hex('0A'), C).



%! 'LWSP'// .
% Linear white space.
%
% ```abnf
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ```
%
% # Note added in RFC 5234
%
% Use of this linear-white-space rule permits lines containing only white
% space that are no longer legal in  mail headers and have caused
% interoperability problems in other contexts.  Do not use when defining mail
% headers and use with caution in other contexts.

'LWSP' --> 'WSP', !, 'LWSP'.
'LWSP' --> 'CRLF', !, 'WSP', 'LWSP'.
'LWSP' --> "".



%! 'OCTET'// .
%! 'OCTET'(?Code:code)// .
% An octect, i.e. 8 bits of data.
%
% # Syntax
%
% ```abnf
% OCTET = %x00-FF   ; 8 bits of data
% ```

'OCTET' --> 'OCTET'(_).
'OCTET'(C) --> between_code_radix(hex('00'), hex('FF'), C).



%! 'SP'// .
%! 'SP'(?Code:code)// .
% The space.
%
% # Syntax
%
% ```abnf
% SP = %x20
% ```

'SP' --> 'SP'(_).
'SP'(C) --> code_radix(hex('20'), C).



%! 'VCHAR'// .
%! 'VCHAR'(?Code:code)// .
% Visible characters.
%
% ```abnf
% VCHAR = %x21-7E   ; visible (printing) characters
% ```

'VCHAR' --> 'VCHAR'(_).
'VCHAR'(C) --> ascii_graphic(C).



%! 'WSP'// .
%! 'WSP'(?Code:code)// .
% Whitesapace, defined as either space or horizontal tab.
%
% ```abnf
% WSP = SP / HTAB   ; white space
% ```

'WSP' --> 'WSP'(_).
'WSP'(C) --> 'SP'(C).
'WSP'(C) --> 'HTAB'(C).
