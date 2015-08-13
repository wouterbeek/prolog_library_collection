:- module(
  uri_char,
  [
    pchar//2, % ?Iri:boolean
              % ?Code:code
    'pct-encoded'//1, % ?Code:between(0,255)
    private//1, % ?Code:code
    'sub-delims'//1, % ?Code:code
    unreserved//2 % ?Iri:boolean
                  % ?Code:code
  ]
).

/** <module> URI Syntax: Characters

Grammar for URI characters.

@author Wouter Beek
@version 2015/08
*/

:- use_module(library(dcg/dcg_abnf)).
:- use_module(library(dcg/dcg_abnf_rules)).
:- use_module(library(dcg/dcg_code)).
:- use_module(library(dcg/dcg_unicode)).
:- use_module(library(math/positional)).





%! 'gen-delims'(?Code:code)// .
% ```abnf
% gen-delims  = ":" / "/" / "?" / "#" / "[" / "]" / "@"
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'gen-delims'(C) --> colon(C).
'gen-delims'(C) --> forward_slash(C).
'gen-delims'(C) --> question_mark(C).
'gen-delims'(C) --> number_sign(C).
'gen-delims'(C) --> square_bracket(C).
'gen-delims'(C) --> at_sign(C).



%! pchar(?Iri:boolean, ?Code:code)// .
% ```abnf
%  pchar =  unreserved / pct-encoded / sub-delims / ":" / "@"
% ipchar = iunreserved / pct-encoded / sub-delims / ":" / "@"
% ```
%
% @compat RFC 3986
% @compat RFC 3987

pchar(I, C) --> unreserved(I, C).
pchar(_, C) --> 'pct-encoded'(C).
pchar(_, C) --> 'sub-delims'(C).
pchar(_, C) --> colon(C).
pchar(_, C) --> at_symbol(C).



%! 'pct-encoded'(?Code:between(0,255))// .
% ```abnf
% pct-encoded = "%" HEXDIG HEXDIG
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'pct-encoded'(C) -->
  "%",
  '#'(2, 'HEXDIG', Ws, []),
  {positional(C, Ws)}.



%! private(?Code:code)// .
% ```abnf
% iprivate = %xE000-F8FF / %xF0000-FFFFD / %x100000-10FFFD
% ```
%
% @compat RFC 3987

private(C) -->
  between_code_radix(hex('E000'),   hex('F8FF'),   C).
private(C) -->
  between_code_radix(hex('F0000'),  hex('FFFFD'),  C).
private(C) -->
  between_code_radix(hex('100000'), hex('10FFFD'), C).



%! reserved(?Code:code)// .
% ```abnf
% reserved = gen-delims / sub-delims
% ```
%
% @compat RFC 3986
% @compat RFC 3987

reserved(C) --> 'gen-delims'(C).
reserved(C) --> 'sub-delims'(C).



%! 'sub-delims'(?Code:code)// .
% ```abnf
% sub-delims =   "!" / "$" / "&" / "'" / "(" / ")"
%              / "*" / "+" / "," / ";" / "="
% ```
%
% @compat RFC 3986
% @compat RFC 3987

'sub-delims'(C) --> exclamation_mark(C).
'sub-delims'(C) --> dollar_sign(C).
'sub-delims'(C) --> ampersand(C).
'sub-delims'(C) --> single_quote(C).
'sub-delims'(C) --> round_bracket(C).
'sub-delims'(C) --> asterisk(C).
'sub-delims'(C) --> plus_sign(C).
'sub-delims'(C) --> comma(C).
'sub-delims'(C) --> semi_colon(C).
'sub-delims'(C) --> equals_sign(C).



%! ucschar(?Code:code)// .
% ```abnf
% ucschar =   %xA0-D7FF / %xF900-FDCF / %xFDF0-FFEF
%           / %x10000-1FFFD / %x20000-2FFFD / %x30000-3FFFD
%           / %x40000-4FFFD / %x50000-5FFFD / %x60000-6FFFD
%           / %x70000-7FFFD / %x80000-8FFFD / %x90000-9FFFD
%           / %xA0000-AFFFD / %xB0000-BFFFD / %xC0000-CFFFD
%           / %xD0000-DFFFD / %xE1000-EFFFD
% ```
%
% @compat RFC 3987

ucschar(C) -->
  between_code_radix(hex('A0'),    hex('D7FF'),  C).
ucschar(C) -->
  between_code_radix(hex('F900'),  hex('FDCF'),  C).
ucschar(C) -->
  between_code_radix(hex('FDF0'),  hex('FFEF'),  C).
ucschar(C) -->
  between_code_radix(hex('10000'), hex('1FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('20000'), hex('2FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('30000'), hex('3FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('40000'), hex('4FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('50000'), hex('5FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('60000'), hex('6FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('70000'), hex('7FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('80000'), hex('8FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('90000'), hex('9FFFD'), C).
ucschar(C) -->
  between_code_radix(hex('A0000'), hex('AFFFD'), C).
ucschar(C) -->
  between_code_radix(hex('B0000'), hex('BFFFD'), C).
ucschar(C) -->
  between_code_radix(hex('C0000'), hex('CFFFD'), C).
ucschar(C) -->
  between_code_radix(hex('D0000'), hex('DFFFD'), C).
ucschar(C) -->
  between_code_radix(hex('E1000'), hex('EFFFD'), C).



%! unreserved(?Iri:boolean, ?Code:code)// .
% ```abnf
%  unreserved = ALPHA / DIGIT / "-" / "." / "_" / "~"
% iunreserved = ALPHA / DIGIT / "-" / "." / "_" / "~" / ucschar
% ```
%
% @compat RFC 3986
% @compat RFC 3987

unreserved(_,    C) --> 'ALPHA'(C).
unreserved(_,    C) --> 'DIGIT'(_, C).
unreserved(_,    C) --> hyphen(C).
unreserved(_,    C) --> dot(C).
unreserved(_,    C) --> underscore(C).
unreserved(_,    C) --> tilde(C).
unreserved(true, C) --> ucschar(C).
