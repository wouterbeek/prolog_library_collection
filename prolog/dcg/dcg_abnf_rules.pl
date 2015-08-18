:- module(
  dcg_abnf_rules,
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
    'HEX'//0,
    'HEX'//1, % ?Weight:between(0,15)
    'HEX'//2, % ?Weight:between(0,15)
              % ?Code:nonneg
    'HEXDIG'//0,
    'HEXDIG'//1, % ?Weight:between(0,15)
    'HEXDIG'//2, % ?Weight:between(0,15)
                % ?Code:code
    'HTAB'//0,
    'HTAB'//1, % ?Code:code
    'LF'//0,
    'LF'//1, % ?Code:code
    'LOALPHA'//0,
    'LOALPHA'//1, % ?Code:nonneg
    'LWS'//0,
    'LWSP'//0,
    'OCTET'//0,
    'OCTET'//1, % ?Code:code
    'SP'//0,
    'SP'//1, % ?Code:code
    'TEXT'//0,
    'TEXT'//1, % ?Code:nonneg
    'UPALPHA'//0,
    'UPALPHA'//1, % ?Code:nonneg
    'VCHAR'//0,
    'VCHAR'//1, % ?Code:code
    'WSP'//0,
    'WSP'//1 % ?Code:code
  ]
).

/** <module> DCG ABNF rules

@author Wouter Beek
@compat [RFC 2234 Section 6.1 Core Rules](https://tools.ietf.org/html/rfc2234)
@compat [RFC 2616 Section 2.2 Basic Rules](http://tools.ietf.org/html/rfc2616)
        except for their definition of 'CHAR'//[0,1]
        which this module considers to be legacy.
@compat [RFC 4234 Appendix B.1 Core Rules](https://tools.ietf.org/html/rfc4234)
@compat [RFC 5234](http://tools.ietf.org/html/rfc5234)
@version 2015/07
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_ascii)).
:- use_module(library(dcg/dcg_code)).





%! '"'// .
%! '"'(?Code:code)// .
% Double quote.
%
% ```abnf
% <"> = <US-ASCII double-quote mark (34)>
% ```
%
% @compat RFC 2616
% @see Terminological variant of 'DQUOTE'//0.

'"' -->
  '"'(_).

'"'(Code) -->
  'DQUOTE'(Code).



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
%
% ## Alternative definition
%
% This is compatible with the way in which RFC 2616 defined this:
%
% ```abnf
% ALPHA = UPALPHA | LOALPHA
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'ALPHA' -->
  'ALPHA'(_).

'ALPHA'(Code) -->
  ascii_letter(Code).



%! 'BIT'// .
%! 'BIT'(?Weight:between(0,1))// .
%! 'BIT'(?Weight:between(0,1), ?Code:code)// .
% A binary digit, i.e. `0` or `1`.
%
% ```abnf
% BIT = "0" / "1"
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234

'BIT' -->
  'BIT'(_).

'BIT'(Weight) -->
  'BIT'(Weight, _).

'BIT'(Weight, Code) -->
  binary_digit(Weight, Code).



%! 'CHAR'// .
%! 'CHAR'(?Code:code)// .
% Any 7-bit US-ASCII character, excluding the NULL character.
%
% # Syntax
%
% ```abnf
% CHAR = %x01-7F   ; any 7-bit US-ASCII character, excluding NUL
% ```
%
% ## Legacy
%
% RFC 2616 defined this to include the NULL character:
%
% ```abnf
% CHAR = <any US-ASCII character (octets 0 - 127)>
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234

'CHAR' -->
  'CHAR'(_).

'CHAR'(Code) -->
  between_code_radix(hex('01'), hex('7F'), Code).



%! 'CR'// .
%! 'CR'(?Code:code)// .
% The carriage return.
%
% ```abnf
% CR = %x0D   ; carriage return
% ```
%
% ## Alternative definition
%
% RFC 2616 defined this is a different but compatible way:
%
% ```abnf
% CR = <US-ASCII CR, carriage return (13)>
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'CR' -->
  'CR'(_).
'CR'(Code) -->
  carriage_return(Code).



%! 'CRLF'// .
% Internet standard newline.
%
% ```abnf
% CRLF = CR LF   ; Internet standard newline
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'CRLF' -->
  'CR',
  'LF'.
'CRLF' -->
  'LF'.



%! 'CTL'// .
%! 'CTL'(?Code:code)// .
% Control character.
%
% # Syntax
%
% ```abnf
% CTL = %x00-1F / %x7F   ; controls
% ```
%
% ## Alternative definition
%
% RFC 2616 defines this in an alternative but compatible way:
%
% ```abnf
% CTL = <any US-ASCII control character (octets 0 - 31) and DEL (127)>
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'CTL' -->
  'CTL'(_).

'CTL'(Code) -->
  control(Code).



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
%
% ## Alternative definition
%
% RFC 2616 defines this in an alternative but compatible way:
%
% ```abnf
% DIGIT = <any US-ASCII digit "0".."9">
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'DIGIT' -->
  'DIGIT'(_).

'DIGIT'(Weight) -->
  'DIGIT'(Weight, _).

'DIGIT'(Weight, Code) -->
  decimal_digit(Weight, Code).



%! 'DQUOTE'// .
%! 'DQUOTE'(?Code:code)// .
% US-ASCII double-quote mark.
%
% ```abnf
% DQUOTE = %x22   ; " (Double Quote)
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234

'DQUOTE' -->
  'DQUOTE'(_).
'DQUOTE'(Code) -->
  double_quote(Code).



%! 'HEX'// .
%! 'HEX'(?Weight:between(0,15))// .
%! 'HEX'(?Weight:between(0,15), ?Code:nonneg)// .
% Case-sensitive notation for hexadecimal digits.
%
% ```abnf
% [RFC 2616]    HEX =     "A" | "B" | "C" | "D" | "E" | "F"
%                       | "a" | "b" | "c" | "d" | "e" | "f"
%                       | DIGIT
% [N-Triples]   HEX ::= [0-9] | [A-F] | [a-f]
% ```
%
% @compat N-Triples 1.1 [162s].
% @compat RFC 2616
% @compat SPARQL 1.0 [171].
% @compat SPARQL 1.1 Query [172].
% @compat Turtle 1.1 [171s].
% @see 'HEXDIG'//[0-2] only supports uppercase letters.

'HEX' -->
  'HEX'(_).

'HEX'(Weight) -->
  'HEX'(Weight, _).

'HEX'(Weight, Code) -->
  hexadecimal_digit(Weight, Code).



%! 'HEXDIG'// .
%! 'HEXDIG'(?Weight:between(0,15))// .
%! 'HEXDIG'(?Weight:between(0,15), ?Code:code)// .
% Uppercase-only notation for hexadecimal digits.
%
% ```abnf
% HEXDIG = DIGIT / "A" / "B" / "C" / "D" / "E" / "F"
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234
% @see 'HEX'//[0-2] supports case-insensitive letters.

'HEXDIG' -->
  'HEXDIG'(_).

'HEXDIG'(Weight) -->
  'HEXDIG'(Weight, _).

'HEXDIG'(Weight, Code) --> 'DIGIT'(Weight, Code).
'HEXDIG'(10, Code) --> a_uppercase(Code).
'HEXDIG'(11, Code) --> b_uppercase(Code).
'HEXDIG'(12, Code) --> c_uppercase(Code).
'HEXDIG'(13, Code) --> d_uppercase(Code).
'HEXDIG'(14, Code) --> e_uppercase(Code).
'HEXDIG'(15, Code) --> f_uppercase(Code).



%! 'HT'// .
%! 'HT'(?Code:nonneg)// .
% The horizontal tab.
%
% ```abnf
% HT = <US-ASCII HT, horizontal-tab (9)>
% ```
%
% @compat RFC 2616
% @see Terminological variant of 'HTAB'//[0,1].

'HT' -->
  'HT'(_).

'HT'(Code) -->
  'HTAB'(Code).



%! 'HTAB'// .
%! 'HTAB'(?Code:code)// .
% The horizontal tab.
%
% ```abnf
% HTAB = %x09   ; horizontal tab
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234

'HTAB' -->
  'HTAB'(_).

'HTAB'(Code) -->
  horizontal_tab(Code).



%! 'LF'// .
%! 'LF'(?Code:code)// .
% The linefeed.
%
% # Syntax
%
% ```abnf
% LF = %x0A   ; linefeed
% ```
%
% ## Alternative definition
%
% RFC 2616 defines this in an alternative but compatible way:
%
% ```abnf
% LF = <US-ASCII LF, linefeed (10)>
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'LF' -->
  'LF'(_).
'LF'(Code) -->
  line_feed(Code).



%! 'LOALPHA'// .
%! 'LOALPHA'(?Code:code)// .
% US-ASCII lowercase letter.
%
% ```abnf
% LOALPHA = <any US-ASCII lowercase letter "a".."z">
% ```
%
% @compat RFC 2616

'LOALPHA' -->
  'LOALPHA'(_).

'LOALPHA'(Code) -->
  ascii_letter_lowercase(Code).



%! 'LWS'// .
%! 'LWS'(?Code:code)// .
% Linear white space.
%
% ```abnf
% LWS = [CRLF] 1*(SP|HT)
% ```
%
% @compat RFC 2616
% @see Terminological variant of 'LWSP'//0.

'LWS' -->
  'LWSP'.



%! 'LWSP'// .
% Linear white space.
%
% ```abnf
% LWSP = *(WSP / CRLF WSP)   ; linear white space (past newline)
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234

'LWSP' -->
  *('LWSP0', []).
'LWSP0' -->
  'LWSP'.
'LWSP0' -->
  'CRLF',
  'WSP'.



%! 'OCTET'// .
%! 'OCTET'(?Code:code)// .
% An octect, i.e. 8 bits of data.
%
% # Syntax
%
% ```abnf
% OCTET = %x00-FF   ; 8 bits of data
% ```
%
% ## Alternative definition
%
% RFC 2616 defines this in an alternative but compatible way:
%
% ```abnf
% OCTET = <any 8-bit sequence of data>
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'OCTET' -->
  'OCTET'(_).

'OCTET'(Code) -->
  between_code_radix(hex('00'), hex('FF'), dec(Code)).



%! 'SP'// .
%! 'SP'(?Code:code)// .
% The space.
%
% # Syntax
%
% ```abnf
% SP = %x20
% ```
%
% ## Alternative definition
%
% RFC 2616 defines this in an alternative but compatible way:
%
% ```abnf
% SP = <US-ASCII SP, space (32)>
% ```
%
% @compat RFC 2234
% @compat RFC 2616
% @compat RFC 4234
% @compat RFC 5234

'SP' -->
  'SP'(_).

'SP'(Code) -->
  space(Code).



%! 'TEXT'// .
%! 'TEXT'(?Code:code)// .
% Used in RFC 2616 for descriptive field contents and values that are
%  not intended to be interpreted by the message parser.
% Words of `*TEXT` MAY contain characters from character sets other than
%  ISO-8859-1 only when encoded according to the rules of RFC 2047.
%
% ```abnf
% TEXT = <any OCTET except CTLs, but including LWS>
% ```
%
% @compat RFC 2616
% @tbd Does this rule make sense?

'TEXT' -->
  'TEXT'(_).

'TEXT'(Code) -->
  'OCTET'(Code),
  {\+ 'CTL'(Code, _, _)}.



%! 'UPALPHA'// .
%! 'UPALPHA'(?Code:code)// .
% US-ASCII uppercase letter.
%
% ```abnf
% UPALPHA = <any US-ASCII uppercase letter "A".."Z">
% ```
%
% @compat RFC 2616

'UPALPHA' -->
  'UPALPHA'(_).

'UPALPHA'(Code) -->
  ascii_letter_uppercase(Code).



%! 'VCHAR'// .
%! 'VCHAR'(?Code:code)// .
% Visible characters.
%
% ```abnf
% VCHAR = %x21-7E   ; visible (printing) characters
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234

'VCHAR' -->
  'VCHAR'(_).

'VCHAR'(Code) -->
  ascii_graphic(Code).



%! 'WSP'// .
%! 'WSP'(?Code:code)// .
% Whitesapace, defined as either space or horizontal tab.
%
% ```abnf
% WSP = SP / HTAB   ; white space
% ```
%
% @compat RFC 2234
% @compat RFC 4234
% @compat RFC 5234

'WSP' -->
  'WSP'(_).

'WSP'(Code) -->
  'SP'(Code).
'WSP'(Code) -->
  'HTAB'(Code).
