:- module(
  rfc2234,
  [
    'BIT'//1, % ?Weight:between(0,1)
    'BIT'//2, % ?Weight:between(0,1)
              % ?Code:code
    'CHAR'//1, % ?Code:code
    'CR'//0,
    'CR'//1, % ?Code:code
    'CRLF'//0,
    'CTL'//0,
    'CTL'//1, % ?Code:Code
    'DQUOTE'//0,
    'HEXDIG'//1, % ?Weight:between(0,15)
    'HEXDIG'//2, % ?Weight:between(0,15)
                 % ?Code:code
    'HTAB'//0,
    'HTAB'//1, % ?Code:code
    'LF'//0,
    'LF'//1, % ?Code:code
    'LWSP'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1, % ?Code:code
    'VCHAR'//1, % ?Code:code
    'WSP'//0
  ]
).
:- reexport(
  library(url/rfc1738_code),
  [
    alpha//1 as 'ALPHA', % ?Code:code
    digit//1 as 'DIGIT', % ?Weight:between(0,9)
    digit//2 as 'DIGIT' % ?Weight:between(0,9)
                        % ?Code:code
  ]
).

/** <module> RFC 2234: Augmented BNF for Syntax Specifications: ABNF

@author Wouter Beek
@compat RFC 2234
@deprecated Use `library(dcg/rfcXXX)` instead.
@version 2015/11
*/

:- use_module(library(dcg/dcg_code)).





%! 'ALPHA'(?Code:code)// .
% RFC 1738 (URL) defines this is a different but compatbile way
% under the name alpha//1.
%
% ```abnf
% ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
% ```



%! 'BIT'(?Weight:between(0,1))// .
%! 'BIT'(?Weight:between(0,1), ?Code:code)// .
% A binary digit, i.e. `0` or `1`.
%
% ```abnf
% BIT = "0" / "1"
% ```

'BIT'(W) --> 'BIT'(W, _).
'BIT'(0, 0'0) --> "0".
'BIT'(1, 0'1) --> "1".



%! 'CHAR'(?Code:code)// .
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% # Syntax
%
% ```abnf
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ```

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
%! 'CRLF'(?Codes:list(code))// .
% Internet standard newline.
%
% ```abnf
% CRLF = CR LF   ; Internet standard newline
% ```

'CRLF' --> 'CRLF'(_).
'CRLF'([X,Y]) --> 'CR'(X), 'LF'(Y).



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



%! 'DIGIT'(?Weight:between(0,9))// .
%! 'DIGIT'(?Weight:between(0,9), ?Code:code)// .
% RFC 1738 (URL) defines this in a different but compatible way
% under the name digit//[1,2].
%
% ```abnf
% DIGIT = %x30-39   ; 0-9
% ```



%! 'DQUOTE'// .
%! 'DQUOTE'(?Code:code)// .
% US-ASCII double-quote mark.
%
% ```abnf
% DQUOTE = %x22   ; " (Double Quote)
% ```

'DQUOTE' --> 'DQUOTE'(_).
'DQUOTE'(C) --> code_radix(hex('22'), C).



%! 'HEXDIG'(?Weight:between(0,15))// .
%! 'HEXDIG'(?Weight:between(0,15), ?Code:code)// .
% Uppercase-only notation for hexadecimal digits.
%
% RFC 1738 (URL) defines a similar grammar rule that include
% lower-case letters as well under the name hex//2.
%
% ```abnf
% HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ```

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
%! 'LWSP'(?Codes:list(code))// .
% Linear white space.
%
% ```abnf
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ```

'LWSP' --> 'LWSP'(_).
'LWSP'([H|T]) --> 'WSP'(H), !, 'LWSP'(T).
'LWSP'([X,Y,Z|T]) --> 'CRLF'([X,Y]), !, 'WSP'(Z), 'LWSP'(T).
'LWSP'([]) --> "".



%! 'OCTET'(?Code:code)// .
% An octect, i.e. 8 bits of data.
%
% # Syntax
%
% ```abnf
% OCTET = %x00-FF   ; 8 bits of data
% ```

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



%! 'VCHAR'(?Code:code)// .
% Visible characters.
%
% ```abnf
% VCHAR = %x21-7E   ; visible (printing) characters
% ```

'VCHAR'(C) --> between_code_radix(hex('21'), hex('7E'), C).



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
