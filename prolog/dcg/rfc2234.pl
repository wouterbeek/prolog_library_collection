:- module(
  rfc2234,
  [
    'ALPHA'//1,  % ?Code:code
    'CHAR'//1,   % ?Code:code
    'CR'//0,
    'CR'//1,     % ?Code:code
    'CRLF'//0,
    'CRLF'//1,   % ?Codes:list(code)
    'CTL'//0,
    'CTL'//1,    % ?Code:Code
    'DIGIT'//1,  % ?Weight
    'DIGIT'//2,  % ?Weight:between(0,9), ?Code:code
    'DQUOTE'//0,
    'HEXDIG'//1, % ?Weight
    'HEXDIG'//2, % ?Weight:between(0,15), ?Code:code
    'HTAB'//0,
    'HTAB'//1,   % ?Code:code
    'LF'//0,
    'LF'//1,     % ?Code:code
    'LWSP'//0,
    'OCTET'//1,  % ?Code:code
    'SP'//0,
    'SP'//1,     % ?Code:code
    'VCHAR'//1,  % ?Code:code
    'WSP'//0,
    'WSP'//1     % ?Code:code
  ]
).

/** <module> RFC 2234 - Augmented BNF for Syntax Specifications (ABNF)

@author Wouter Beek
@deprecated dcg/rfc4234
@see https://tools.ietf.org/html/rfc2234
@version 2017/05-2017/06
*/





%! 'ALPHA'(?Code:code)// .
%
% RFC 1738 (URL) defines this is a different but compatbile way
% under the name alpha//1.
%
% ```abnf
% ALPHA = %x41-5A / %x61-7A   ; A-Z / a-z
% ```

'ALPHA'(C) -->
  [C],
  {once(between(0x41, 0x5A, C) ; between(0x61, 0x7A, C))}.



%! 'BIT'(?Code:code)// .
%
% A binary digit, i.e. `0` or `1`.
%
% ```abnf
% BIT = "0" / "1"
% ```

'BIT'(0'0) --> "0".
'BIT'(0'1) --> "1".



%! 'CHAR'(?Code:code)// .
%
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% # Syntax
%
% ```abnf
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ```

'CHAR'(C) -->
  [C],
  {between(1, 127, C)}.



%! 'CR'// .
%! 'CR'(?Code:code)// .
%
% The carriage return.
%
% ```abnf
% CR = %x0D   ; carriage return
% ```

'CR' --> 'CR'(_).
'CR'(13) --> [13].



%! 'CRLF'// .
%! 'CRLF'(?Codes:list(code))// .
%
% Internet standard newline.
%
% ```abnf
% CRLF = CR LF   ; Internet standard newline
% ```

'CRLF' -->
  'CRLF'(_).


'CRLF'([Code1,Code2]) -->
  'CR'(Code1),
  'LF'(Code2).



%! 'CTL'// .
%! 'CTL'(?Code:code)// .
%
% Control character.
%
% # Syntax
%
% ```abnf
% CTL = %x00-1F / %x7F   ; controls
% ```

'CTL' -->
  'CTL'(_).


'CTL'(C) -->
  [C],
  {(between(0, 31, C) ; C = 127)}.



%! 'DIGIT'(?Weight:between(0,9))// .
%! 'DIGIT'(?Weight:between(0,9), ?Code:code)// .
%
% RFC 1738 (URL) defines this in a different but compatible way
% under the name digit//[1,2].
%
% ```abnf
% DIGIT = %x30-39   ; 0-9
% ```

'DIGIT'(Weight) -->
  'DIGIT'(Weight, _).


'DIGIT'(0, 0'0) --> [0x30].
'DIGIT'(1, 0'1) --> [0x31].
'DIGIT'(2, 0'2) --> [0x32].
'DIGIT'(3, 0'3) --> [0x33].
'DIGIT'(4, 0'4) --> [0x34].
'DIGIT'(5, 0'5) --> [0x35].
'DIGIT'(6, 0'6) --> [0x36].
'DIGIT'(7, 0'7) --> [0x37].
'DIGIT'(8, 0'8) --> [0x38].
'DIGIT'(9, 0'9) --> [0x39].



%! 'DQUOTE'// .
%! 'DQUOTE'(?Code:code)// .
%
% US-ASCII double-quote mark.
%
% ```abnf
% DQUOTE = %x22   ; " (Double Quote)
% ```

'DQUOTE' -->
  'DQUOTE'(_).


'DQUOTE'(0'") --> "\"". %"



%! 'HEXDIG'(?Weight:between(0,15))// .
%! 'HEXDIG'(?Weight:between(0,15), ?Code:code)// .
%
% RFC 1738 (URL) defines a similar grammar rule that include
% lower-case letters as well under the name hex//2.
%
% ```abnf
% HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ```

'HEXDIG'(Weight) -->
  'HEXDIG'(Weight, _).


'HEXDIG'(Weight, C)    --> 'DIGIT'(Weight, C).
'HEXDIG'(10, 0'A) --> "A".
'HEXDIG'(11, 0'B) --> "B".
'HEXDIG'(12, 0'C) --> "C".
'HEXDIG'(13, 0'D) --> "D".
'HEXDIG'(14, 0'E) --> "E".
'HEXDIG'(15, 0'F) --> "F".



%! 'HTAB'// .

'HTAB' --> 'HTAB'(_).


%! 'HTAB'(?Code:code)// .
% The horizontal tab.
%
% ```abnf
% HTAB = %x09   ; horizontal tab
% ```

'HTAB'(0x09) --> [0x09].



%! 'LF'// .

'LF' --> 'LF'(_).


%! 'LF'(?Code:code)// .
% The linefeed.
%
% # Syntax
%
% ```abnf
% LF = %x0A   ; linefeed
% ```

'LF'(0x0A) --> [0x0A].



%! 'LWSP'// .
%! 'LWSP'(?Codes:list(code))// .
%
% Linear white space.
%
% ```abnf
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ```

'LWSP' -->
  'LWSP'(_).


'LWSP'([H|T])     --> 'WSP'(H),      !,           'LWSP'(T).
'LWSP'([X,Y,Z|T]) --> 'CRLF'([X,Y]), !, 'WSP'(Z), 'LWSP'(T).
'LWSP'([]) --> "".



%! 'OCTET'(?Code:code)// .
%
% An octect, i.e. 8 bits of data.
%
% # Syntax
%
% ```abnf
% OCTET = %x00-FF   ; 8 bits of data
% ```

'OCTET'(C) -->
  [C],
  {between(0, 255, C)}.



%! 'SP'// .
%! 'SP'(?Code:code)// .
%
% The space.
%
% # Syntax
%
% ```abnf
% SP = %x20
% ```

'SP' -->
  'SP'(_).


'SP'(0x20) --> [0x20].



%! 'VCHAR'(?Code:code)// .
%
% Visible characters.
%
% ```abnf
% VCHAR = %x21-7E   ; visible (printing) characters
% ```

'VCHAR'(C) -->
  [C],
  {between(33, 126, C)}.



%! 'WSP'// .
%! 'WSP'(?Code:code)// .
%
% Whitesapace, defined as either space or horizontal tab.
%
% ```abnf
% WSP = SP / HTAB   ; white space
% ```

'WSP' -->
  'WSP'(_).


'WSP'(C) --> 'SP'(C).
'WSP'(C) --> 'HTAB'(C).
